# Original five-bank panel: data dictionary and extraction protocol

## Scope

The target is the quarterly panel used in Adrian and Shin (2010), Table 3. The five firms and bank-level sample windows reported in the paper are:

| Bank | First quarter | Last quarter |
|---|---:|---:|
| Bear Stearns | 1997 Q1 | 2008 Q1 |
| Goldman Sachs | 1999 Q2 | 2008 Q1 |
| Lehman Brothers | 1993 Q2 | 2008 Q1 |
| Merrill Lynch | 1991 Q1 | 2008 Q1 |
| Morgan Stanley | 1997 Q2 | 2008 Q1 |

The estimation sample is smaller because some explanatory variables, particularly repo financing and VaR, are not available for every bank-quarter.

## Target dataset

Create one observation per bank-quarter. Preserve source-reported values separately from derived variables.

| Variable | Definition | Extraction / calculation |
|---|---|---|
| `bank` | Bank name | Fixed identifier |
| `cik` | SEC Central Index Key | SEC registrant identifier |
| `report_date` | Fiscal quarter-end | Filing cover page / metadata |
| `filing_form` | Filing type | 10-Q or 10-K; amendments separately identified |
| `total_assets` | Total assets | Consolidated balance sheet; record units |
| `total_liabilities` | Total liabilities | Consolidated balance sheet; record units |
| `book_equity` | Book equity | Use paper-consistent balance-sheet definition |
| `leverage` | Assets divided by book equity | Derived after validating both inputs |
| `repo_financing` | Repo borrowing / securities sold under agreements to repurchase | Record exact filing line item and aggregation |
| `trading_var` | Trading value-at-risk | Record horizon, confidence level, and aggregation |
| `source_accession` | SEC accession number | Filing metadata |
| `source_url` | Filing URL | SEC archive link |
| `source_page` | Page or note reference | Manual extraction log |
| `source_units` | Reported units | For example, USD millions |
| `extraction_method` | XBRL or manual filing extraction | Record method per field |
| `notes` | Comparability and missingness notes | Required for non-standard definitions |

Candidate derived variables for Table 3 (confirm exact transformation against the paper before final estimation):

- Leverage growth: quarterly change in log leverage.
- Lagged leverage: lagged leverage term as specified in the table.
- Asset growth: quarterly change in log total assets.
- Repo growth: quarterly change in log repo financing.
- VaR growth: quarterly change in log trading VaR.

Do not calculate log growth if either endpoint is zero, negative, missing, or based on a non-comparable definition. Do not interpolate missing observations.

## Extraction protocol

1. Run `scripts/01_build_sec_filing_inventory.R` to collect filing metadata and URLs.
2. Inspect the actual filing, not just current companyfacts values. SEC XBRL reporting was first required in 2009, after most of the paper's sample; companyfacts alone cannot be assumed to supply the full historical panel.
3. Extract balance-sheet items and note disclosures. Repo and VaR may require manual extraction from filing text and tables.
4. Record each source item, page/note, units, and mapping decision in an extraction log.
5. Keep source-reported values immutable in a raw extraction file. Build the analysis-ready panel separately.
6. Reconcile variable definitions, dates, and sample counts with the paper's Table 1 and Table 3 before estimation.

## Important limitations

- Banks have different reporting calendars. Use actual report dates, not assumed calendar quarter-ends; document any mapping to calendar quarters.
- Annual 10-K values do not replace missing quarterly 10-Q observations.
- Repo financing may appear in a balance-sheet line, a note, or a detailed schedule. Do not combine differently defined items without a documented rule.
- VaR disclosures can change horizon, confidence level, and aggregation. Preserve the reported definition and flag breaks.
- This is a replication dataset to be assembled from filings, not a ready-made panel. Never fabricate observations to fill gaps.
