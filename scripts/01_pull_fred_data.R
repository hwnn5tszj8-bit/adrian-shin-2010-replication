# ==============================================================================
# Script 01: Automated FRED Data Pull (Flow of Funds Z.1, VIX, S&P 500)
# Paper: Adrian & Shin (2010), "Liquidity and Leverage"
# ==============================================================================

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

# Read one published FRED CSV at a time, with an explicit date and value schema.
read_fred_series <- function(series) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series)
  data <- tryCatch(
    read_csv(url, col_types = cols(observation_date = col_date(), .default = col_double()),
             na = c("", "NA", ".")),
    error = function(error) stop("FRED download failed for ", series, ": ", conditionMessage(error))
  )
  if (!identical(names(data), c("observation_date", series)) || !any(is.finite(data[[series]]))) {
    stop("FRED returned an invalid or empty series: ", series)
  }
  rename(data, date = observation_date)
}

fred <- set_names(map(fred_series, read_fred_series), fred_series)
safe_log <- function(x) log(if_else(is.finite(x) & x > 0, x, NA_real_))
log_change <- function(x) 100 * (safe_log(x) - lag(safe_log(x)))

# ------------------------------------------------------------------------------
# 2. Merge Quarterly Balance Sheet Data & Compute Leverage Growth
# ------------------------------------------------------------------------------
z1_names <- fred_series[!fred_series %in% c("VIXCLS", "SP500")]

z1_raw <- fred[z1_names] %>%
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
    d_bd_assets = log_change(bd_assets),
    d_bd_lev    = log_change(bd_lev),
    
    d_cb_assets = log_change(cb_assets),
    d_cb_lev    = log_change(cb_lev),
    
    d_hh_assets = log_change(hh_assets),
    d_hh_lev    = log_change(hh_lev),
    
    d_nf_assets = log_change(nf_assets),
    d_nf_lev    = log_change(nf_lev),
    
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
  fred[["VIXCLS"]],
  fred[["SP500"]],
  by = "date"
) %>%
  arrange(date) %>%
  mutate(
    sp500_ret = 100 * (log(SP500) - log(lag(SP500)))
  )

write_csv(market_daily, "data/raw/fred_vix_sp500_daily.csv")

cat("Success! Saved cleaned Z.1 sector data and daily VIX/SP500 data.\n")