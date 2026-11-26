"""Extract traceable consolidated statement values from the cached SEC filings."""

import csv
from datetime import date
import hashlib
from html import unescape
from pathlib import Path
import re

MANIFEST = Path("data/processed/sec_filing_manifest.csv")
DOCUMENTS = Path("data/raw/sec_filings/documents")
PANEL = Path("data/processed/five_bank_balance_sheets.csv")
AUDIT = Path("output/logs/five_bank_extraction_audit.csv")

STATEMENT = re.compile(
    r"\b(?:condensed\s+)?consolidated\s+(?:balance\s+sheets?|(?:statements?\s+of\s+)?financial\s+condition)\b",
    re.I,
)
OTHER_STATEMENT = re.compile(
    r"\b(?:consolidated|consolidating)\s+(?:statements?\s+of\s+(?:income|earnings|operations|cash flows)|"
    r"statements?\s+of\s+changes\s+in|balance sheets? of)\b", re.I
)
ASSETS = re.compile(r"^total\s+assets\b", re.I)
EQUITY = re.compile(
    r"^(?:total\s+)?(?:stockholders?|shareholders?|members?|partners?)'?(?:s)?\s+equity\b"
    r"|^total\s+equity\b", re.I
)
TOTAL = re.compile(
    r"^total\s+liabilities.*\b(?:equity|capital)\b|^total\s+liabilities\s+and\s+stockholders", re.I
)
LIABILITIES = re.compile(r"^total\s+liabilities[. :]*$", re.I)
NUMBER = re.compile(r"\(?[-−]?\d[\d,]*(?:\.\d+)?\)?")
UNIT = re.compile(r"(?:\bin\s+|\$\s*)(millions|thousands)\b", re.I)
MONTHS = (
    "January February March April May June July August September October November December".split()
)


def filing_documents(raw, form):
    # The audited statements may be incorporated into a 10-K from Exhibit 13.
    docs = re.findall(r"<DOCUMENT>(.*?)</DOCUMENT>", raw, flags=re.I | re.S)
    if not docs:
        yield form, raw
        return
    for doc in docs:
        kind = re.search(r"(?im)^<TYPE>\s*([^\r\n]+)", doc)
        if kind and (kind[1].strip() == form or (form == "10-K" and kind[1].strip() in {"EX-13", "EX-13.1"})):
            yield kind[1].strip(), re.split(r"<TEXT>", doc, maxsplit=1, flags=re.I)[-1]


def statement_lines(raw):
    # Replace rendered table cells with delimiters, keeping legacy EDGAR
    # plain-text table lines and their spacing.
    raw = re.sub(r"<!--.*?-->|<(?:script|style)\b.*?</(?:script|style)>", "", raw, flags=re.I | re.S)
    raw = re.sub(r"<tr\b[^>]*>(.*?)</tr\s*>", lambda m: "\n" + re.sub(r"</(?:p|div|pre|center)>|<br\b[^>]*>", " ",
                                                   re.sub(r"\s+", " ", m[1]), flags=re.I) + "\n",
                 raw, flags=re.I | re.S)
    raw = re.sub(r"</t[dh]\s*>", "\t", raw, flags=re.I)
    raw = re.sub(r"</(?:p|div|table|pre|center|caption)>|<br\b[^>]*>", "\n", raw, flags=re.I)
    raw = re.sub(r"<[^>]+>", "", raw)
    raw = unescape(raw).translate(str.maketrans({"\xa0": " ", "’": "'", "‘": "'"}))
    return [line.strip() for line in raw.splitlines() if line.strip()]


def fields(line):
    cells = [cell.strip() for cell in re.split(r"\t+| {2,}|\.{3,}", line) if cell.strip()]
    if not cells:
        return "", []
    first_value = next((i for i, cell in enumerate(cells)
                        if cell.startswith("$") or NUMBER.fullmatch(cell)), len(cells))
    label = " ".join(cells[:first_value])
    numbers = []
    for cell in cells[first_value:]:
        for item in cell.replace("$", "").split():
            if NUMBER.fullmatch(item):
                numbers.append(float(item.replace(",", "").replace("(", "-").replace(")", "").replace("−", "-")))
    return label, numbers


def has_report_date(lines, report_date):
    # The financial statements put the current period in the first column.
    # Some issuers give only the month and year, or split dates across rows.
    date_text = " ".join(" ".join(lines).split())
    months = "|".join(rf"{name}|{name[:3]}\.?" for name in MONTHS)
    first_month = re.search(rf"\b({months})(?=\s|$)", date_text, re.I)
    first_year = re.search(r"\b(?:19|20)\d{2}\b", date_text)
    if not first_month or not first_year:
        return False
    if first_month[1][:3].lower() != MONTHS[report_date.month - 1][:3].lower():
        return False
    day = re.match(r"\s+(\d{1,2})\b", date_text[first_month.end():])
    return int(first_year[0]) == report_date.year and (not day or int(day[1]) == report_date.day)


def candidates(lines, report_date):
    parsed = [fields(line) for line in lines]
    for header, line in enumerate(lines):
        heading = " ".join(lines[header:header + 2]) if not STATEMENT.search(line) else line
        match = STATEMENT.match(heading)
        if not match or OTHER_STATEMENT.search(heading):
            continue
        if not re.fullmatch(r"[\s.:()—–-]*(?:(?:unaudited|continued|assets)[\s.:()—–-]*)*", heading[match.end():], re.I):
            continue
        # Statements sometimes put assets and liabilities on facing pages.
        limit = min(header + 180, len(lines))
        for eq in range(header + 1, limit):
            label, equity = parsed[eq]
            if OTHER_STATEMENT.search(lines[eq]) and len(lines[eq]) < 110 and not STATEMENT.search(lines[eq]):
                break
            if not equity or equity[0] <= 0 or (label and not EQUITY.match(label)):
                continue
            totals = [i for i in range(eq + 1, min(eq + 12, limit))
                      if TOTAL.match(parsed[i][0]) and parsed[i][1] and parsed[i][1][0] > equity[0]]
            if not totals:
                # Early Goldman filings leave the final asset, liability and
                # equity totals unlabeled. Require the independent identity
                # for the current column before using an unlabeled total.
                totals = [i for i in range(eq + 1, min(eq + 3, limit))
                          if not parsed[i][0] and parsed[i][1] and parsed[i][1][0] > equity[0]]
            for total in totals:
                assets = parsed[total][1][0]
                asset_rows = [i for i in range(header + 1, eq)
                              if parsed[i][1] and parsed[i][1][0] == assets and ASSETS.match(parsed[i][0])]
                if not asset_rows:
                    asset_rows = [i for i in range(header + 1, eq)
                                  if parsed[i][1] and parsed[i][1][0] == assets and not parsed[i][0]]
                if not asset_rows:
                    continue
                if not label or not parsed[total][0]:
                    comparisons = [entry for entry in parsed[asset_rows[-1] + 1:eq]
                                   if entry[1] and (not entry[0] or LIABILITIES.fullmatch(entry[0]))]
                    if not any(abs(assets - vals[0] - equity[0]) <= 1 for _, vals in comparisons):
                        continue
                asset = asset_rows[-1]
                if not has_report_date(lines[header + 1:min(asset, header + 16)], report_date):
                    continue
                units = UNIT.search(" ".join(lines[header:asset]))
                if not units:
                    continue
                scale = 1 if units[1].lower() == "millions" else 0.001
                liabilities = next(
                    (vals[0] * scale for lbl, vals in parsed[asset + 1:eq] if vals and LIABILITIES.fullmatch(lbl)),
                    None,
                )
                yield {
                    "total_assets_mn": round(assets * scale, 6),
                    "book_equity_mn": round(equity[0] * scale, 6),
                    "reported_liabilities_mn": liabilities,
                    "source_units": units[1].lower(),
                    "statement_heading": heading,
                    "date_context": " | ".join(lines[header + 1:min(asset, header + 11)]),
                    "assets_row": lines[asset],
                    "equity_row": lines[eq],
                    "balance_total_row": lines[total],
                    "normalized_asset_line": asset + 1,
                }


def write_csv(path, rows, columns):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=columns, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)


def filing_matches(raw, row):
    report_date = date.fromisoformat(row["report_date"])
    return [
        dict(candidate, document_type=kind)
        for kind, document in filing_documents(raw.decode("cp1252", errors="replace"), row["form"])
        for candidate in candidates(statement_lines(document), report_date)
    ]


def main():
    panel, audit = [], []
    with MANIFEST.open(newline="", encoding="utf-8") as stream:
        manifest = list(csv.DictReader(stream))
    for row in manifest:
        source = {key: row[key] for key in ("bank", "form", "report_date", "accession_number", "source_url")}
        path = Path(row["local_file"])
        if not path.is_relative_to(DOCUMENTS) or ".." in path.parts:
            raise ValueError(f"Unexpected filing path: {path}")
        if not path.is_file():
            audit.append(dict(source, status="Missing filing", candidate_count=0))
            continue
        raw = path.read_bytes()
        matches = filing_matches(raw, row)
        unique = {(c["total_assets_mn"], c["book_equity_mn"]): c for c in matches}
        status = "Matched statement" if len(unique) == 1 else "Conflicting matches" if unique else "No strict match"
        audit.append(dict(source, status=status, candidate_count=len(unique)))
        if len(unique) == 1:
            panel.append(dict(source, **next(iter(unique.values())), sha256=hashlib.sha256(raw).hexdigest()))
    if not panel:
        raise SystemExit("No matched statements. Check the SEC cache and filing manifest.")
    write_csv(PANEL, panel, list(panel[0]))
    write_csv(AUDIT, audit, list(audit[0]))


if __name__ == "__main__":
    main()
