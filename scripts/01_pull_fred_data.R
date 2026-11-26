# ==============================================================================
# Script 01: Automated FRED Data Pull (Flow of Funds Z.1, VIX, S&P 500)
# Paper: Adrian & Shin (2010), "Liquidity and Leverage"
# ==============================================================================

library(quantmod)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(lubridate)

# Ensure folders exist
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Define FRED Series IDs (Board of Governors Z.1 Financial Accounts)
# ------------------------------------------------------------------------------
fred_series <- c(
  # Security Brokers & Dealers (Table L.129)
  "BOGZ1FL664090005Q", # Total Financial Assets
  "BOGZ1FL664190005Q", # Total Liabilities
  "BOGZ1FL664194005Q", # Total Liabilities and Equity
  
  # Commercial Banks / U.S.-Chartered Depository Institutions (Table L.111)
  "BOGZ1FL764090005Q", # Total Financial Assets
  "BOGZ1FL764190005Q", # Total Liabilities
  
  # Bank Holding Companies (Table L.112 - for Figure 3.1 scaling)
  "BOGZ1FL734090005Q", # Total Financial Assets
  
  # Households and Nonprofit Organizations (Table B.101)
  "TABSHNO",           # Total Assets
  "TLBSHNO",           # Total Liabilities
  
  # Nonfinancial Corporate Business (Table B.103)
  "TABSNNCB",          # Total Assets
  "TLBSNNCB",          # Total Liabilities
  
  # Daily Market Variables (Section 4)
  "VIXCLS",            # CBOE Volatility Index (Daily)
  "SP500"              # S&P 500 Index (Daily)
)

# Download all series from FRED
getSymbols(fred_series, src = "FRED", auto.assign = TRUE)

# Helper function to convert xts object to clean tibble
xts_to_df <- function(series_name) {
  obj <- get(series_name)
  tibble(
    date = index(obj),
    value = as.numeric(obj[, 1])
  ) %>% rename(!!series_name := value)
}

# ------------------------------------------------------------------------------
# 2. Merge Quarterly Balance Sheet Data & Compute Leverage Growth
# ------------------------------------------------------------------------------
z1_names <- fred_series[!fred_series %in% c("VIXCLS", "SP500")]

z1_raw <- z1_names %>%
  map(xts_to_df) %>%
  reduce(full_join, by = "date") %>%
  arrange(date)

# Save raw Z.1 pull
write_csv(z1_raw, "data/raw/fred_z1_raw.csv")

# Clean and compute log quarterly growth rates (matching Figures 2.1 - 2.4)
# Note: TABSHNO/TABSNNCB are in Billions, BOGZ1 series are in Millions (ratios are unit-free)
z1_clean <- z1_raw %>%
  mutate(
    # Broker-Dealers
    bd_assets = BOGZ1FL664090005Q,
    bd_equity = BOGZ1FL664090005Q - BOGZ1FL664190005Q,
    bd_lev    = bd_assets / bd_equity,
    
    # Commercial Banks
    cb_assets = BOGZ1FL764090005Q,
    cb_equity = BOGZ1FL764090005Q - BOGZ1FL764190005Q,
    cb_lev    = cb_assets / cb_equity,
    
    # Households
    hh_assets = TABSHNO,
    hh_equity = TABSHNO - TLBSHNO,
    hh_lev    = hh_assets / hh_equity,
    
    # Nonfinancial Corporates
    nf_assets = TABSNNCB,
    nf_equity = TABSNNCB - TLBSNNCB,
    nf_lev    = nf_assets / nf_equity
  ) %>%
  mutate(
    # Quarterly percentage growth rates: 100 * (log(X_t) - log(X_{t-1}))
    d_bd_assets = 100 * (log(bd_assets) - log(lag(bd_assets))),
    d_bd_lev    = 100 * (log(bd_lev)    - log(lag(bd_lev))),
    
    d_cb_assets = 100 * (log(cb_assets) - log(lag(cb_assets))),
    d_cb_lev    = 100 * (log(cb_lev)    - log(lag(cb_lev))),
    
    d_hh_assets = 100 * (log(hh_assets) - log(lag(hh_assets))),
    d_hh_lev    = 100 * (log(hh_lev)    - log(lag(hh_lev))),
    
    d_nf_assets = 100 * (log(nf_assets) - log(lag(nf_assets))),
    d_nf_lev    = 100 * (log(nf_lev)    - log(lag(nf_lev))),
    
    # Sample indicator: Original Paper (1963-2006) vs. Post-GFC Extension
    sample_period = case_when(
      date >= as.Date("1963-01-01") & date <= as.Date("2006-12-31") ~ "Original (1963-2006)",
      date >= as.Date("2009-01-01") ~ "Post-GFC (2009-Present)",
      date >= as.Date("2007-01-01") ~ "GFC Crisis (2007-2008)",
      TRUE ~ "Before original sample"
    )
  )

write_csv(z1_clean, "data/clean/z1_sector_leverage.csv")

# ------------------------------------------------------------------------------
# 3. Merge Daily VIX and S&P 500 Data
# ------------------------------------------------------------------------------
market_daily <- full_join(
  xts_to_df("VIXCLS"),
  xts_to_df("SP500"),
  by = "date"
) %>%
  arrange(date) %>%
  mutate(
    sp500_ret = 100 * (log(SP500) - log(lag(SP500)))
  )

write_csv(market_daily, "data/raw/fred_vix_sp500_daily.csv")

cat("Success! Saved cleaned Z.1 sector data and daily VIX/SP500 data.\n")