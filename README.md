# Liquidity and Leverage: Replication and Post-GFC Extension of Adrian & Shin (2010)

An empirical replication and post-2008 extension in **R** of Tobias Adrian and Hyun Song Shin (2010), *"Liquidity and Leverage,"* *Journal of Financial Intermediation*, 19(3), 418–437.

## Overview

Adrian and Shin (2010) demonstrate that marked-to-market financial intermediaries—specifically **U.S. Security Broker-Dealers**—actively manage their balance sheets so that leverage is strongly **procyclical**: dealers expand debt (primarily via repurchase agreements) during asset price booms and contract balance sheets during downturns. This repository replicates the paper's core cross-sector balance sheet regressions and weekly Primary Dealer repo–VIX forecasting tests, then extends the analysis through **2026** to evaluate how post-2008 capital and leverage regulations (Basel III / Supplementary Leverage Ratio) altered broker-dealer balance-sheet dynamics.

---

## Repository Structure

```text
adrian-shin-2010-replication/
├── data/
│   ├── raw/                        # Raw FRED Z.1, daily VIX/S&P 500, and NY Fed FR 2004 CSVs
│   └── clean/                      # Cleaned quarterly sector panel & weekly dealer-VIX panel
├── scripts/
│   ├── 01_pull_fred_data.R         # Downloads Z.1 sector balance sheets & CBOE VIX/S&P 500 via quantmod
│   ├── 02_sector_leverage_analysis.R # Replicates Figs 2.1–2.4, Fig 3.1, Table 3, & Post-GFC regime tests
│   └── 03_repo_vix_forecasting.R   # Splices 1998–2026 NY Fed FR 2004 repo data; runs Tables 4–5 & Fig 4.1
├── output/
│   ├── figures/                    # High-resolution PNG plots (300 DPI)
│   └── tables/                     # Regression & summary statistics tables (CSV)
└── README.md