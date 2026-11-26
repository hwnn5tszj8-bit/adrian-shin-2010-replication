# Five-bank data

The SEC filing sample follows the institutions in Adrian and Shin’s Table 1. Its filing coverage is uneven across banks and periods.

| Bank | Paper start | Paper end |
| --- | --- | --- |
| Bear Stearns | 1997 Q1 | 2008 Q1 |
| Goldman Sachs | 1999 Q2 | 2008 Q1 |
| Lehman Brothers | 1993 Q2 | 2008 Q1 |
| Merrill Lynch | 1991 Q1 | 2008 Q1 |
| Morgan Stanley | 1997 Q2 | 2008 Q1 |

## Inputs and audit

| File | Contents |
| --- | --- |
| `data/processed/sec_filing_manifest.csv` | Bank, filing form, fiscal report date, SEC accession, source URL, and cache path for each of 222 filings. |
| `data/processed/five_bank_balance_sheets.csv` | One row per unambiguous statement match. `total_assets_mn` and `book_equity_mn` are in millions of dollars. `reported_liabilities_mn` is present when the statement labels that line explicitly. |
| `output/logs/five_bank_extraction_audit.csv` | Match status and the number of distinct numeric pairs per filing. |
| `output/tables/five_bank_coverage.csv` | Matched rows and usable regression quarters by firm. |
| `output/tables/five_bank_pilot_comparison.csv` | Matched values alongside the preliminary pilot transcriptions. |

The statement extract records the heading, column context, monetary units, three source rows, normalized line number, source document type, and SHA-256 of the downloaded filing. The normalized line number is computed by the extractor from the identified filing document. Some 10-Ks incorporate the financial statements from Exhibit 13. Those rows identify the exhibit in `document_type`.

Matching requires a consolidated statement, first-column period consistent with the filing report date, stated dollar units, positive equity and matching asset and liabilities-plus-equity totals. A separately reported liabilities subtotal must reconcile where an unlabeled total is used. Conflicting values and unmatched dates stay out of the panel. A review of the original source is warranted when extending the supported filing formats.

## Estimation sample

The R analysis maps fiscal report dates to calendar quarters. Multiple matched filings in the same bank-quarter are excluded from growth calculations, along with pairs separated by a missing quarter or dates less than 70 or more than 115 days apart. Leverage is assets divided by book equity. The outcome is the quarterly log change in leverage, and the regressors are the preceding log level of leverage and the current quarterly log change in assets. The panel uses a bank intercept and calendar-quarter HAC covariance for the reported estimates.

The figures and coefficients use only the matched filing subset. Repo borrowing and trading VaR require a separate extraction and validation effort. The older files retain the initial candidate review and the pilot work for comparison.
