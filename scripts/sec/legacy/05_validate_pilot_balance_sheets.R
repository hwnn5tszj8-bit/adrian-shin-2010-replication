# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 05: Reconcile pilot balance sheet observations
# ============================================================

library(dplyr)
library(readr)
library(tibble)


pilot <- tribble(
  ~bank, ~report_date, ~total_assets, ~total_liabilities, ~book_equity, ~source_form, ~source_table,
  
  "Bear Stearns", "2007-11-30",
  395362, 383569, 11793,
  "10-Q", "Comparative consolidated balance sheet",
  
  "Bear Stearns", "2008-02-29",
  398995, 387099, 11896,
  "10-Q", "Consolidated balance sheet",
  
  "Goldman Sachs", "2007-11-30",
  1119796, 1076996, 42800,
  "10-K", "Table 114 / 219",
  
  "Goldman Sachs", "2008-02-29",
  1189006, 1146377, 42629,
  "10-Q", "Table 6",
  
  "Lehman Brothers", "2007-11-30",
  691277, 668577, 22700,
  "10-K", "Table 98",
  
  "Lehman Brothers", "2008-02-29",
  786035, 761203, 24832,
  "10-Q", "Table 46 - consolidated column",
  
  "Merrill Lynch", "2008-03-28",
  1042054, 1005512, 36542,
  "10-Q", "Table 125",
  
  "Morgan Stanley", "2007-11-30",
  1045409, NA_real_, 31269,
  "10-K", "Table 42",
  
  "Morgan Stanley", "2008-02-29",
  1090896, NA_real_, 33280,
  "10-Q", "Table 250"
)

# Calculate leverage and validation indicators

pilot <- pilot %>%
  mutate(
    report_date = as.Date(report_date),
    year = lubridate::year(report_date),
    quarter = lubridate::quarter(report_date),
    report_quarter = paste0(year, "Q", quarter),
    leverage = total_assets / book_equity,
    accounting_error = total_assets -
      total_liabilities - book_equity,
    accounting_check = case_when(
      is.na(total_liabilities) ~ "Not independently checked",
      abs(accounting_error) <= 1 ~ "Passed",
      TRUE ~ "Failed"
    ),
    source_units = "USD millions - preliminary",
    extraction_method = "Pilot extraction and reconciliation",
    verified = FALSE
  ) %>%
  arrange(bank, report_date)

# Calculate preliminary quarterly growth

pilot <- pilot %>%
  group_by(bank) %>%
  mutate(
    quarter_id = year * 4 + quarter,
    
    asset_growth = if_else(
      quarter_id - lag(quarter_id) == 1,
      total_assets / lag(total_assets) - 1,
      NA_real_
    ),
    
    equity_growth = if_else(
      quarter_id - lag(quarter_id) == 1,
      book_equity / lag(book_equity) - 1,
      NA_real_
    ),
    
    leverage_growth = if_else(
      quarter_id - lag(quarter_id) == 1,
      leverage / lag(leverage) - 1,
      NA_real_
    )
  ) %>%
  ungroup()

# Save reconciled pilot dataset

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  pilot,
  "data/processed/pilot_balance_sheets_reconciled.csv",
  na = ""
)

# Display results

cat("\n========================================\n")
cat("PILOT DATASET RECONCILED\n")
cat("========================================\n")

print(
  pilot %>%
    select(
      bank,
      report_quarter,
      total_assets,
      book_equity,
      leverage,
      accounting_check
    ),
  n = Inf,
  width = Inf
)

cat("\nQuarterly growth rates (%):\n")

print(
  pilot %>%
    select(
      bank,
      report_quarter,
      asset_growth,
      equity_growth,
      leverage_growth
    ) %>%
    mutate(
      across(
        ends_with("_growth"),
        ~ round(.x * 100, 2)
      )
    ),
  n = Inf,
  width = Inf
)

cat("\nTotal observations:", nrow(pilot), "\n")
cat("Banks represented:", n_distinct(pilot$bank), "\n")
cat("\nPilot dataset saved successfully.\n")