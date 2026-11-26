# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 02: Prepare original bank financial extraction dataset
# ============================================================

library(dplyr)
library(readr)

setwd("~/GitHub/adrian-shin-2010-replication")

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

# Load SEC filing inventory

filings <- read_csv(
  "data/raw/sec_filings/original_five_bank_filing_inventory.csv",
  show_col_types = FALSE
)

# Retain original annual and quarterly reports

extraction <- filings %>%
  filter(form %in% c("10-K", "10-Q")) %>%
  arrange(bank, report_date, filing_date) %>%
  select(
    bank,
    cik,
    report_date,
    report_quarter,
    filing_date,
    form,
    accession_number,
    filing_url
  ) %>%
  mutate(
    total_assets = NA_real_,
    total_liabilities = NA_real_,
    book_equity = NA_real_,
    repo_financing = NA_real_,
    reverse_repo = NA_real_,
    trading_var = NA_real_,
    source_units = NA_character_,
    balance_sheet_reference = NA_character_,
    repo_reference = NA_character_,
    var_reference = NA_character_,
    var_confidence_level = NA_real_,
    var_horizon_days = NA_real_,
    extraction_method = NA_character_,
    verified = FALSE,
    notes = NA_character_
  )

# Save template without overwriting existing extracted data

output_file <- "data/raw/sec_filings/bank_financial_extraction.csv"

if (file.exists(output_file)) {
  stop("Extraction file already exists. Not overwriting existing data.")
}

write_csv(
  extraction,
  output_file,
  na = ""
)

# Display results

cat("\nFinancial extraction template created.\n")
cat("Filing records:", nrow(extraction), "\n")
cat("Saved to:", output_file, "\n\n")

print(
  extraction %>%
    count(bank, form),
  n = Inf
)