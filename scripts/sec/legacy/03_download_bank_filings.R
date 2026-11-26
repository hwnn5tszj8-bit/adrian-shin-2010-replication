# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 03: Download original SEC financial reports
# ============================================================

library(httr2)
library(dplyr)
library(readr)


# 1. Load SEC configuration


sec_user_agent <- Sys.getenv("SEC_USER_AGENT")

if (!nzchar(sec_user_agent)) {
  stop("SEC_USER_AGENT is missing.")
}

# 2. Load filing inventory

filings <- read_csv(
  "data/raw/sec_filings/original_five_bank_filing_inventory.csv",
  show_col_types = FALSE
)

# 3. Create download folder

dir.create(
  "data/raw/sec_filings/documents",
  recursive = TRUE,
  showWarnings = FALSE
)

# 4. Select pilot sample
# One annual and one quarterly report per bank

pilot_filings <- filings %>%
  filter(
    form %in% c("10-K", "10-Q"),
    !is.na(filing_url),
    filing_url != ""
  ) %>%
  group_by(bank, form) %>%
  slice_max(
    order_by = report_date,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

print(
  pilot_filings %>%
    select(bank, form, report_date),
  n = Inf
)

# 5. Download function

download_filing <- function(url, destination) {
  
  if (file.exists(destination) &&
      file.info(destination)$size > 0) {
    
    return("Already downloaded")
  }
  
  Sys.sleep(0.5)
  
  tryCatch({
    
    response <- request(url) %>%
      req_user_agent(sec_user_agent) %>%
      req_retry(max_tries = 3) %>%
      req_timeout(120) %>%
      req_perform()
    
    writeBin(
      resp_body_raw(response),
      destination
    )
    
    return("Downloaded")
    
  }, error = function(e) {
    
    return(
      paste("ERROR:", conditionMessage(e))
    )
    
  })
}

# 6. Download pilot filings

download_log <- vector(
  "list",
  nrow(pilot_filings)
)

for (i in seq_len(nrow(pilot_filings))) {
  
  row <- pilot_filings[i, ]
  
  bank_folder <- gsub(
    " ",
    "_",
    row$bank
  )
  
  folder <- file.path(
    "data/raw/sec_filings/documents",
    bank_folder
  )
  
  dir.create(
    folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  filename <- paste0(
    row$accession_number,
    "_",
    basename(row$primary_document)
  )
  
  destination <- file.path(
    folder,
    filename
  )
  
  cat(
    "\nDownloading:",
    row$bank,
    row$form,
    as.character(row$report_date),
    "\n"
  )
  
  status <- download_filing(
    row$filing_url,
    destination
  )
  
  cat(status, "\n")
  
  download_log[[i]] <- tibble(
    bank = row$bank,
    form = row$form,
    report_date = row$report_date,
    accession_number = row$accession_number,
    source_url = row$filing_url,
    local_file = destination,
    status = status,
    file_size_bytes = if (
      file.exists(destination)
    ) {
      file.info(destination)$size
    } else {
      NA_real_
    }
  )
}

# 7. Save download log

download_results <- bind_rows(download_log)

dir.create(
  "output/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  download_results,
  "output/logs/sec_document_download_log.csv"
)

# 8. Display results

cat("\n========================================\n")
cat("SEC DOCUMENT DOWNLOAD COMPLETE\n")
cat("========================================\n")

print(
  download_results %>%
    select(
      bank,
      form,
      report_date,
      status,
      file_size_bytes
    ),
  n = Inf,
  width = Inf
)

cat("\nDownload summary:\n")

print(
  download_results %>%
    count(status)
)