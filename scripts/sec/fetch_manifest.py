"""Download the fixed SEC filing sample for the balance-sheet extraction."""
import csv
import os
from pathlib import Path
import time
from urllib.parse import urlsplit
from urllib.request import Request, urlopen

from extract_consolidated_panel import DOCUMENTS, MANIFEST


def main():
    user_agent = os.environ.get("SEC_USER_AGENT", "").strip()
    if "@" not in user_agent:
        raise SystemExit("Set SEC_USER_AGENT to your name or organization and contact email.")
    with MANIFEST.open(newline="", encoding="utf-8") as source:
        filings = list(csv.DictReader(source))
    failures = []
    for filing in filings:
        path = Path(filing["local_file"])
        url = urlsplit(filing["source_url"])
        if (not path.is_relative_to(DOCUMENTS) or ".." in path.parts
                or url.scheme != "https" or url.netloc != "www.sec.gov"
                or not url.path.startswith("/Archives/edgar/data/")):
            raise ValueError("Unexpected filing URL or cache path")
        if path.is_file() and path.stat().st_size:
            continue
        time.sleep(0.30)
        try:
            request = Request(url.geturl(), headers={"User-Agent": user_agent})
            with urlopen(request, timeout=30) as response:
                data = response.read()
            if len(data) < 100 or b"Undeclared Automated Tool" in data[:10000]:
                raise ValueError("Received an empty response or an SEC access page")
            path.parent.mkdir(parents=True, exist_ok=True)
            temporary = path.with_suffix(path.suffix + ".part")
            temporary.write_bytes(data)
            temporary.replace(path)
        except (OSError, ValueError) as error:
            failures.append((filing["accession_number"], str(error)))
    for accession, error in failures:
        print(f"{accession}: {error}")
    if failures:
        raise SystemExit(f"{len(failures)} filings failed. Rerun to download uncached filings.")
    print(f"Cached {len(filings)} filings.")


if __name__ == "__main__":
    main()
