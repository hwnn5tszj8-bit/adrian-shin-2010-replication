# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Original Replication
# Script 01: Download SEC Filing Inventory
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(lubridate)
library(tidyr)

# SEC contact information
# ------------------------------------------------------------


sec_user_agent <- Sys.getenv("SEC_USER_AGENT")

if (!nzchar(sec_user_agent)) {
  stop("SEC_USER_AGENT is missing.")
}

# 4. Create project folders
# ------------------------------------------------------------

dir.create(
  "data/raw/sec_submissions",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/raw/sec_filings",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "output/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

# 5. Define the original five-bank sample
# ------------------------------------------------------------

banks <- tibble(
  bank = c(
    "Bear Stearns",
    "Goldman Sachs",
    "Lehman Brothers",
    "Merrill Lynch",
    "Morgan Stanley"
  ),
  cik = c(
    "0000777001",
    "0000886982",
    "0000806085",
    "0000065100",
    "0000895421"
  ),
  start_year = c(1997, 1999, 1993, 1991, 1997),
  start_quarter = c(1, 2, 2, 1, 2),
  end_year = rep(2008, 5),
  end_quarter = rep(1, 5)
)

print(banks)

# 6. Define SEC download function
# ------------------------------------------------------------

download_sec_json <- function(url, destination) {
  
  Sys.sleep(0.5)
  
  response <- request(url) %>%
    req_user_agent(sec_user_agent) %>%
    req_retry(max_tries = 3) %>%
    req_timeout(60) %>%
    req_perform()
  
  raw_json <- resp_body_string(response)
  
  writeLines(
    raw_json,
    con = destination,
    useBytes = TRUE
  )
  
  jsonlite::fromJSON(
    raw_json,
    simplifyVector = TRUE
  )
}

# 7. Define filing metadata extraction function
# ------------------------------------------------------------

extract_filings <- function(x, bank_name, bank_cik) {
  
  if (is.null(x)) {
    return(tibble())
  }
  
  if (length(x$accessionNumber) == 0) {
    return(tibble())
  }
  
  n <- length(x$accessionNumber)
  
  get_field <- function(field) {
    
    value <- x[[field]]
    
    if (is.null(value) || length(value) != n) {
      return(rep(NA_character_, n))
    }
    
    as.character(value)
  }
  
  tibble(
    bank = rep(bank_name, n),
    cik = rep(bank_cik, n),
    accession_number = get_field("accessionNumber"),
    filing_date = get_field("filingDate"),
    report_date = get_field("reportDate"),
    form = get_field("form"),
    primary_document = get_field("primaryDocument")
  )
}

# 8. Download filing histories
# ------------------------------------------------------------

all_filings <- list()

for (i in seq_len(nrow(banks))) {
  
  bank_name <- banks$bank[i]
  bank_cik <- banks$cik[i]
  
  cat("\n========================================\n")
  cat("Downloading:", bank_name, "\n")
  cat("CIK:", bank_cik, "\n")
  cat("========================================\n")
  
  url <- paste0(
    "https://data.sec.gov/submissions/CIK",
    bank_cik,
    ".json"
  )
  
  destination <- file.path(
    "data/raw/sec_submissions",
    paste0("CIK", bank_cik, ".json")
  )
  
  submissions <- download_sec_json(
    url,
    destination
  )
  
  recent <- extract_filings(
    submissions$filings$recent,
    bank_name,
    bank_cik
  )
  
  bank_filings <- recent
  
  historical_files <- submissions$filings$files
  
  if (
    is.data.frame(historical_files) &&
    nrow(historical_files) > 0
  ) {
    
    for (j in seq_len(nrow(historical_files))) {
      
      old_name <- historical_files$name[j]
      
      cat("Downloading historical index:", old_name, "\n")
      
      old_url <- paste0(
        "https://data.sec.gov/submissions/",
        old_name
      )
      
      old_destination <- file.path(
        "data/raw/sec_submissions",
        old_name
      )
      
      old_json <- download_sec_json(
        old_url,
        old_destination
      )
      
      old_filings <- extract_filings(
        old_json,
        bank_name,
        bank_cik
      )
      
      bank_filings <- bind_rows(
        bank_filings,
        old_filings
      )
    }
  }
  
  all_filings[[bank_name]] <- bank_filings
  
  cat(
    "Total filing records retrieved:",
    nrow(bank_filings),
    "\n"
  )
}

# 9. Combine and clean filing metadata
# ------------------------------------------------------------

filings <- bind_rows(all_filings) %>%
  mutate(
    filing_date = as.Date(
      filing_date,
      format = "%Y-%m-%d"
    ),
    report_date = as.Date(
      report_date,
      format = "%Y-%m-%d"
    )
  ) %>%
  filter(
    form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A"),
    !is.na(report_date)
  ) %>%
  distinct(bank, accession_number, .keep_all = TRUE) %>%
  mutate(
    year = lubridate::year(report_date),
    quarter = lubridate::quarter(report_date),
    quarter_id = year * 4 + quarter,
    report_quarter = paste0(year, "Q", quarter)
  ) %>%
  left_join(
    banks,
    by = c("bank", "cik")
  ) %>%
  filter(
    quarter_id >= start_year * 4 + start_quarter,
    quarter_id <= end_year * 4 + end_quarter
  ) %>%
  mutate(
    accession_clean = gsub(
      "-",
      "",
      accession_number
    ),
    cik_numeric = as.character(as.integer(cik)),
    filing_url = if_else(
      !is.na(primary_document) & primary_document != "",
      paste0(
        "https://www.sec.gov/Archives/edgar/data/",
        cik_numeric,
        "/",
        accession_clean,
        "/",
        primary_document
      ),
      NA_character_
    )
  ) %>%
  arrange(bank, report_date, filing_date) %>%
  select(
    bank,
    cik,
    report_date,
    report_quarter,
    filing_date,
    form,
    accession_number,
    primary_document,
    filing_url
  )

# 10. Save filing inventory
# ------------------------------------------------------------

write_csv(
  filings,
  "data/raw/sec_filings/original_five_bank_filing_inventory.csv",
  na = ""
)

# 11. Construct expected quarterly sample
# ------------------------------------------------------------

expected_quarters <- bind_rows(
  lapply(seq_len(nrow(banks)), function(i) {
    
    first <- banks$start_year[i] * 4 +
      banks$start_quarter[i] - 1
    
    last <- banks$end_year[i] * 4 +
      banks$end_quarter[i] - 1
    
    periods <- seq(first, last)
    
    tibble(
      bank = banks$bank[i],
      report_quarter = paste0(
        periods %/% 4,
        "Q",
        periods %% 4 + 1
      )
    )
  })
)

# 12. Check filing coverage and missing quarters
# ------------------------------------------------------------

observed_coverage <- filings %>%
  filter(form %in% c("10-K", "10-Q")) %>%
  count(
    bank,
    report_quarter,
    name = "n_filings"
  )

coverage <- expected_quarters %>%
  left_join(
    observed_coverage,
    by = c("bank", "report_quarter")
  ) %>%
  mutate(
    n_filings = coalesce(n_filings, 0L),
    status = if_else(
      n_filings > 0,
      "Available",
      "Missing"
    )
  ) %>%
  arrange(bank, report_quarter)

write_csv(
  coverage,
  "output/logs/sec_filing_coverage_by_bank_quarter.csv"
)

# 13. Summarise coverage by bank
# ------------------------------------------------------------

coverage_summary <- coverage %>%
  group_by(bank) %>%
  summarise(
    expected_quarters = n(),
    available_quarters = sum(n_filings > 0),
    missing_quarters = sum(n_filings == 0),
    coverage_pct = round(
      100 * available_quarters / expected_quarters,
      2
    ),
    .groups = "drop"
  )

write_csv(
  coverage_summary,
  "output/logs/sec_filing_coverage_summary.csv"
)

# 14. Display final results
# ------------------------------------------------------------

cat("\n========================================\n")
cat("SEC FILING INVENTORY COMPLETE\n")
cat("========================================\n")

cat("Total relevant filings:", nrow(filings), "\n\n")

print(coverage_summary)

cat("\nFiles created:\n")

cat(
  "data/raw/sec_filings/",
  "original_five_bank_filing_inventory.csv\n",
  sep = ""
)

cat(
  "output/logs/",
  "sec_filing_coverage_by_bank_quarter.csv\n",
  sep = ""
)

cat(
  "output/logs/",
  "sec_filing_coverage_summary.csv\n",
  sep = ""
)

cat("\nScript 01 completed successfully.\n")