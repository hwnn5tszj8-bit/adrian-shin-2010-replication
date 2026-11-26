# Liquidity and Leverage

An R replication and post-crisis comparison for Adrian and Shin (2010), *Liquidity and Leverage*, Journal of Financial Intermediation, 19(3), 418–437. Figure and table references below follow the [New York Fed staff report](https://www.newyorkfed.org/medialibrary/media/research/staff_reports/sr328.pdf).

## Current results

- **Sector leverage (Figures 2.1–2.4).** The Federal Reserve Financial Accounts data reproduce the main contrast. Over 1963–2006, the estimated asset-growth slope is −0.121 for households, 0.343 for commercial banks (HAC p = 0.431), and 0.893 for broker-dealers. The post-crisis dealer slope is 0.668 through 2026 Q2. The comparison is descriptive. A pooled interaction test is needed to test the change in slopes.
- **Five-bank leverage (Table 3).** The sector regression uses aggregate data. Table 3 uses quarterly SEC filings for five investment banks and includes firm fixed effects and lagged leverage. The `build/original-bank-panel` branch contains an SEC filing inventory builder and extraction protocol. The completed bank-quarter observations and estimates are pending.
- **Dealer repo and implied volatility (Table 5, columns i–ii).** In the committed results, the repo coefficient is 0.0062 (HAC p = 0.506) for the pre-crisis subset and 0.0045 (HAC p = 0.109) for the post-crisis subset. The paper reports a negative coefficient using a 1990–April 2008 sample and Wednesday-to-Tuesday average VIX. Our NY Fed data start in January 1998 and the script uses Wednesday closing VIX (or the latest available close within three days). The accompanying summary and scatterplot are related analyses for Table 4 and Figure 4.1. The S&P 500 snapshot lacks the historical coverage needed for the paper's realized-volatility and risk-premium comparison.

## Assignment scope

Use the sector leverage figures and post-crisis comparison as the main result. For the firm-level model, prioritize the assets-and-leverage specification in Table 3, column (ii), and add the repo specification where filings support comparable observations. For the weekly model, prioritize the VIX-only benchmark and its repo-growth counterpart in Table 5, columns (i)–(ii). Match the weekly VIX window, report the overlapping historical dates, and then apply the same specification to the recent sample. The assignment calls for key results, a recent-sample re-estimation, and policy implications in a 30-minute presentation.

## Run from the repository root

Install the R packages once with `install.packages(c("tidyverse", "lubridate", "sandwich", "lmtest", "quantmod"))`. Run `Rscript scripts/02_sector_leverage_analysis.R` and `Rscript scripts/03_repo_vix_forecasting.R` with the committed CSV snapshots. Run `Rscript scripts/01_pull_fred_data.R` first to refresh the FRED data over the network. The scripts write figures and tables to `output/`. The weekly script uses the NY Fed Primary Dealer data under `data/raw/`.
