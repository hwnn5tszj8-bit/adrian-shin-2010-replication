# Liquidity and Leverage

A focused replication and recent-sample extension of Tobias Adrian and Hyun Song Shin (2010), *Liquidity and Leverage*, for Advanced Financial Economics. Figure and table numbers follow the [New York Fed staff report](https://www.newyorkfed.org/medialibrary/media/research/staff_reports/sr328.pdf).

## Results

| Analysis | Result |
| --- | --- |
| Sector leverage, Figs. 2.1–2.4 | Over 1963–2006 the asset-growth slope is −0.121 for households and +0.893 for broker-dealers. The nonfinancial and commercial-bank slopes are statistically inconclusive. |
| Five-bank leverage, Table 3 (i)–(ii) | The matched SEC filing subset gives a Table 3 (ii) asset-growth coefficient of **0.789** (95% CI 0.464–1.114), versus **0.83** in the paper. The lagged leverage coefficient is −0.113, versus −0.04. The reconstruction covers all five firms and uses 139 regression observations, compared with 235 in the paper. |
| Post-crisis dealer sector | The simple slope falls to **0.668** over 2009 Q1–2026 Q2. The change is −0.225 (HAC p = 0.019). In the Table 3-style aggregate model with lagged leverage, the slope falls from **0.902 to 0.590** (difference p = 0.001). These comparisons are descriptive. |
| Repo and VIX, Table 5 (i)–(ii) | Published repo totals and Wed–Tue VIX averages yield a repo coefficient of **+0.0062** (p = 0.404) in the 1998–2008 overlap and **+0.0104** (p = 0.575) in 2009–2013. The 2013–2026 agency MBS repo proxy yields **−0.0241** (p = 0.441). The paper’s historical result is negative and significant. |

The presentation and speaker notes are in [presentation.pptx](presentation/presentation.pptx). The analysis CSVs and figures are under [output](output/).

## Run the analysis

Run from the repository root. The analysis uses the committed data snapshots and needs no network connection.

```r
install.packages(c("dplyr", "ggplot2", "readr", "lubridate"))
```

```text
Rscript --vanilla scripts/run_analysis.R
```

To rebuild the five-bank extraction, use Python 3.9+ with `SEC_USER_AGENT` set to your name or organization and contact email:

```text
python scripts/sec/fetch_manifest.py
python scripts/sec/extract_consolidated_panel.py
Rscript --vanilla scripts/04_five_bank_leverage_analysis.R
```

The fetch script uses the dated filing manifest in `data/processed/` and reuses local downloads. The extraction audit includes unmatched filings. The earlier SEC inventory and pilot R scripts live under `scripts/sec/legacy/` and are reference workflows. The older candidate CSVs and extraction logs document the preliminary work. The main analysis reads `five_bank_balance_sheets.csv`.

For an optional live FRED refresh, install `quantmod` and `purrr` and run `Rscript --vanilla scripts/01_pull_fred_data.R`. This updates the checked-in market and Financial Accounts snapshots. The NY Fed Primary Dealer extract is a separate input.

## Methods and scope

- **Sector model.** Leverage is total assets divided by assets less liabilities, using the Financial Accounts series in the pull script. Changes are quarterly log differences. Tests allow the intercept and slope to vary across periods. The aggregate dynamic model uses the same regressors as the assets-and-leverage firm specification and is an extension for the broader sector. Differences can reflect changes in reporting, sector composition, and market conditions.
- **Investment banks.** The manifest contains 222 filings. Statement matching checks a consolidated heading, current reporting column, monetary units, and the same amount on both sides of the balance sheet. For unlabeled liability/equity totals, it also requires the accounting identity with a separately reported subtotal. The 178 matched rows include source URLs, source rows, and document digests. Eight pilot transcriptions agree exactly. The Lehman 2007-11-30 10-K reports $691,063m in assets and $22,490m in equity, and the extraction uses those amounts. The panel requires consecutive, uniquely identified calendar quarters and fits bank fixed effects. The reduced sample and coverage differences across firms limit the comparison with the paper. The repo and VaR columns are outside this reconstruction.
- **Weekly forecasts.** Repo growth is 100 times the weekly log change in the reported financing series. At Wednesday t, use the repo change ending on t and the average VIX from the preceding Wednesday–Tuesday window to forecast the change to the window beginning on t. The benchmark uses the same observations as the repo model. Reporting-series and sample breaks interrupt growth. The NY Fed’s posted history begins in January 1998. Pre-2013 results use published totals. The agency residential MBS series gives a transparent proxy after the reporting redesign. The modern proxy has narrower financing coverage. The analysis covers implied-volatility columns (i)–(ii). Historical realized volatility and the volatility risk premium require a longer daily equity-price series.
- **Inference.** The regressions use a four-period Bartlett HAC covariance, with gaps preserved on the actual quarterly or weekly calendar. The firm-level estimator sums regression scores across firms within each quarter before forming lagged covariance terms. This accommodates contemporaneous dependence in the five-bank sample. Inference choices and the accounting construction of leverage limit causal interpretation.

## Sources

- [Financial Accounts of the United States (Federal Reserve)](https://www.federalreserve.gov/releases/z1/) and [FRED](https://fred.stlouisfed.org/)
- [Primary Dealer Statistics (New York Fed)](https://www.newyorkfed.org/markets/counterparties/primary-dealers-statistics)
- [VIXCLS daily series (CBOE via FRED)](https://fred.stlouisfed.org/series/VIXCLS)
- [SEC EDGAR](https://www.sec.gov/edgar/search/) with individual accessions in the filing manifest
