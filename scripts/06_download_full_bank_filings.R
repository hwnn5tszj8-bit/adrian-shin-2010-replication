
# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 06: Download full historical SEC filing sample
# ============================================================

library(httr2)
library(dplyr)
library(readr)

# ------------------------------------------------------------
# 1. Project configuration
# ------------------------------------------------------------

setwd("~/GitHub/adrian-shin-2010-replication")

if (file.exists(path.expand("~/.Renviron"))) {
  readRenviron(path.expand("~/.Renviron"))
}

sec_user_agent <- Sys.getenv("SEC_USER_AGENT")

if (!nzchar(sec_user_agent)) {
  stop("SEC_USER_AGENT is missing.")
}

inventory_path <- paste0(
  "data/raw/sec_filings/",
  "original_five_bank_filing_inventory.csv"
)

document_dir <- "data/raw/sec_filings/documents"
log_dir <- "output/logs"

log_path <- file.path(
  log_dir,
  "full_sec_document_download_log.csv"
)

summary_path <- file.path(
  log_dir,
  "full_sec_document_download_summary.csv"
)

failed_path <- file.path(
  log_dir,
  "full_sec_document_download_failed.csv"
)

dir.create(
  document_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  log_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 2. Load the ORIGINAL SEC filing inventory
# ------------------------------------------------------------

filings <- read_csv(
  inventory_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

required_columns <- c(
  "bank",
  "cik",
  "form",
  "report_date",
  "accession_number",
  "primary_document",
  "filing_url"
)

missing_columns <- setdiff(
  required_columns,
  names(filings)
)

if (length(missing_columns) > 0) {
  stop(
    "Missing inventory columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 3. Select all original 10-K and 10-Q filings
# ------------------------------------------------------------

full_filings <- filings %>%
  filter(form %in% c("10-K", "10-Q")) %>%
  distinct(accession_number, .keep_all = TRUE) %>%
  arrange(bank, report_date, accession_number)

# Reconstruct missing URLs when metadata is available.
# For older filings without a primary document name,
# use the complete SEC submission text file.

full_filings <- full_filings %>%
  mutate(
    cik_clean = sub("^0+", "", cik),
    
    accession_clean = gsub(
      "-", "", accession_number
    ),
    
    reconstructed_url = case_when(
      
      is.na(cik_clean) |
        !nzchar(cik_clean) |
        is.na(accession_number) ~ NA_character_,
      
      !is.na(primary_document) &
        nzchar(primary_document) ~ paste0(
          "https://www.sec.gov/Archives/edgar/data/",
          cik_clean, "/",
          accession_clean, "/",
          primary_document
        ),
      
      TRUE ~ paste0(
        "https://www.sec.gov/Archives/edgar/data/",
        cik_clean, "/",
        accession_number, ".txt"
      )
    ),
    
    source_url = case_when(
      !is.na(filing_url) &
        nzchar(trimws(filing_url)) ~ filing_url,
      
      TRUE ~ reconstructed_url
    ),
    
    fallback_url = paste0(
      "https://www.sec.gov/Archives/edgar/data/",
      cik_clean, "/",
      accession_number, ".txt"
    )
  )

if (nrow(full_filings) == 0) {
  stop("No original 10-K or 10-Q filings found.")
}

cat("\nHistorical filings selected:",
    nrow(full_filings), "\n\n")

print(
  full_filings %>%
    count(bank, form),
  n = Inf
)

if (nrow(full_filings) != 222) {
  warning(
    "The current inventory contains ",
    nrow(full_filings),
    " original reports rather than the expected 222."
  )
}

# ------------------------------------------------------------
# 4. Helper: validate a downloaded document
# ------------------------------------------------------------

valid_document <- function(path) {
  
  if (is.na(path) || !file.exists(path)) {
    return(FALSE)
  }
  
  size <- file.info(path)$size
  
  if (is.na(size) || size < 1000) {
    return(FALSE)
  }
  
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  
  header <- readBin(
    con,
    what = "raw",
    n = 8192
  )
  
  header <- header[header != as.raw(0)]
  
  header <- rawToChar(header)
  
  error_page <- grepl(
    paste(
      "undeclared automated tool",
      "request rate threshold exceeded",
      "<title>access denied",
      "<title>forbidden",
      "<title>gateway timeout",
      "<title>service unavailable",
      sep = "|"
    ),
    header,
    ignore.case = TRUE,
    useBytes = TRUE
  )
  
  !error_page
}

# ------------------------------------------------------------
# 5. Helper: download one SEC document
# ------------------------------------------------------------

download_filing <- function(url, destination) {
  
  if (valid_document(destination)) {
    
    return(list(
      status = "cached",
      http_status = NA_integer_,
      message = "Existing document reused"
    ))
  }
  
  if (
    is.na(url) ||
    !grepl(
      "^https://www\\.sec\\.gov/Archives/edgar/data/",
      url
    )
  ) {
    
    return(list(
      status = "failed",
      http_status = NA_integer_,
      message = "Missing or invalid SEC URL"
    ))
  }
  
  temporary_file <- paste0(destination, ".part")
  
  on.exit(
    unlink(temporary_file),
    add = TRUE
  )
  
  for (attempt in seq_len(4)) {
    
    unlink(temporary_file)
    
    Sys.sleep(0.5)
    
    response <- tryCatch({
      
      request(url) %>%
        req_user_agent(sec_user_agent) %>%
        req_timeout(120) %>%
        req_error(
          is_error = function(resp) FALSE
        ) %>%
        req_perform(path = temporary_file)
      
    }, error = function(e) e)
    
    if (inherits(response, "error")) {
      
      http_status <- NA_integer_
      error_message <- conditionMessage(response)
      
    } else {
      
      http_status <- resp_status(response)
      
      error_message <- paste(
        "HTTP", http_status
      )
      
      if (
        http_status == 200L &&
        valid_document(temporary_file)
      ) {
        
        if (file.exists(destination)) {
          unlink(destination)
        }
        
        moved <- file.rename(
          temporary_file,
          destination
        )
        
        if (moved && valid_document(destination)) {
          
          return(list(
            status = "downloaded",
            http_status = 200L,
            message = "Download successful"
          ))
        }
        
        error_message <- "Unable to save document"
      }
      
      # Stop immediately on SEC access restrictions.
      
      if (http_status %in% c(403L, 429L)) {
        
        return(list(
          status = "restricted",
          http_status = http_status,
          message = error_message
        ))
      }
      
      # Do not retry permanent HTTP errors.
      
      if (http_status %in% c(
        400L, 401L, 404L, 410L
      )) {
        break
      }
      
      if (http_status == 200L) {
        error_message <- "Invalid document or save failure"
      }
    }
    
    if (attempt < 4) {
      Sys.sleep(5 * 2^(attempt - 1))
    }
  }
  
  list(
    status = "failed",
    http_status = http_status,
    message = error_message
  )
}

# ------------------------------------------------------------
# 6. Initialize persistent download log
# ------------------------------------------------------------

download_log <- tibble(
  bank = character(),
  form = character(),
  report_date = character(),
  accession_number = character(),
  source_url = character(),
  local_file = character(),
  status = character(),
  http_status = integer(),
  file_size_bytes = double(),
  message = character()
)

if (file.exists(log_path)) {
  
  previous_log <- read_csv(
    log_path,
    col_types = cols(
      .default = col_character(),
      http_status = col_integer(),
      file_size_bytes = col_double()
    ),
    show_col_types = FALSE
  )
  
  if (all(names(download_log) %in% names(previous_log))) {
    download_log <- previous_log
  }
}

# ------------------------------------------------------------
# 7. Download the full historical sample
# ------------------------------------------------------------

total_filings <- nrow(full_filings)

cat("\nStarting full historical download...\n\n")

for (i in seq_len(total_filings)) {
  
  row <- full_filings[i, ]
  
  bank_folder <- gsub(
    " ", "_", row$bank
  )
  
  folder <- file.path(
    document_dir,
    bank_folder
  )
  
  dir.create(
    folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # Use the same filename convention as Script 03.
  
  document_name <- if (
    !is.na(row$primary_document) &&
    nzchar(row$primary_document)
  ) {
    basename(row$primary_document)
  } else {
    "submission.txt"
  }
  
  destination <- file.path(
    folder,
    paste0(
      row$accession_number,
      "_",
      document_name
    )
  )
  
  fallback_destination <- file.path(
    folder,
    paste0(
      row$accession_number,
      "_submission.txt"
    )
  )
  
  used_url <- row$source_url
  
  # Reuse a previously downloaded fallback document.
  
  if (
    !valid_document(destination) &&
    valid_document(fallback_destination)
  ) {
    
    destination <- fallback_destination
    used_url <- row$fallback_url
  }
  
  result <- download_filing(
    used_url,
    destination
  )
  
  # If the primary document cannot be downloaded,
  # try the complete SEC submission text file.
  
  if (
    result$status == "failed" &&
    !is.na(row$cik_clean) &&
    nzchar(row$cik_clean) &&
    !is.na(row$accession_number) &&
    !is.na(row$fallback_url) &&
    !identical(used_url, row$fallback_url)
  ) {
    
    used_url <- row$fallback_url
    destination <- fallback_destination
    
    result <- download_filing(
      used_url,
      destination
    )
  }
  
  # Record the outcome.
  
  log_row <- tibble(
    bank = row$bank,
    form = row$form,
    report_date = row$report_date,
    accession_number = row$accession_number,
    source_url = used_url,
    local_file = destination,
    status = result$status,
    http_status = result$http_status,
    
    file_size_bytes = if (
      file.exists(destination)
    ) {
      as.numeric(file.info(destination)$size)
    } else {
      NA_real_
    },
    
    message = result$message
  )
  
  download_log <- download_log %>%
    filter(
      accession_number != row$accession_number
    ) %>%
    bind_rows(log_row)
  
  # Save progress after every filing.
  
  write_csv(
    download_log,
    log_path,
    na = ""
  )
  
  cat(
    sprintf(
      "[%03d/%03d] %-16s %-4s %-10s %s\n",
      i,
      total_filings,
      row$bank,
      row$form,
      ifelse(
        is.na(row$report_date),
        "Unknown",
        row$report_date
      ),
      result$status
    )
  )
  
  if (result$status == "restricted") {
    
    warning(
      "SEC access restriction detected. ",
      "Progress saved. Resume later."
    )
    
    break
  }
}

# ------------------------------------------------------------
# 8. Construct complete download status
# ------------------------------------------------------------

current_log <- full_filings %>%
  select(
    bank,
    form,
    report_date,
    accession_number
  ) %>%
  left_join(
    download_log %>%
      select(
        accession_number,
        source_url,
        local_file,
        status,
        http_status,
        file_size_bytes,
        message
      ),
    by = "accession_number"
  ) %>%
  mutate(
    status = coalesce(status, "pending")
  )

# Confirm successful documents still exist.

available_files <- vapply(
  current_log$local_file,
  valid_document,
  logical(1)
)

current_log <- current_log %>%
  mutate(
    status = if_else(
      status %in% c("cached", "downloaded") &
        !available_files,
      "failed",
      status
    )
  )

# ------------------------------------------------------------
# 9. Download summary by bank
# ------------------------------------------------------------

download_summary <- current_log %>%
  group_by(bank) %>%
  summarise(
    expected_filings = n(),
    
    cached = sum(
      status == "cached"
    ),
    
    downloaded = sum(
      status == "downloaded"
    ),
    
    failed = sum(
      status %in% c("failed", "restricted")
    ),
    
    pending = sum(
      status == "pending"
    ),
    
    available = sum(
      status %in% c("cached", "downloaded")
    ),
    
    coverage_pct = round(
      100 * available / expected_filings,
      2
    ),
    
    .groups = "drop"
  )

failed_filings <- current_log %>%
  filter(
    !status %in% c("cached", "downloaded")
  )

write_csv(
  download_summary,
  summary_path
)

write_csv(
  failed_filings,
  failed_path,
  na = ""
)

# ------------------------------------------------------------
# 10. Display final results
# ------------------------------------------------------------

cat("\n========================================\n")
cat("FULL HISTORICAL SEC DOWNLOAD SUMMARY\n")
cat("========================================\n\n")

print(
  download_summary,
  n = Inf,
  width = Inf
)

cat(
  "\nTotal identified filings:",
  sum(download_summary$expected_filings),
  "\n"
)

cat(
  "Total available documents:",
  sum(download_summary$available),
  "\n"
)

cat(
  "Failed downloads:",
  sum(download_summary$failed),
  "\n"
)

cat(
  "Pending downloads:",
  sum(download_summary$pending),
  "\n"
)

cat(
  "Overall document coverage:",
  round(
    100 * sum(download_summary$available) /
      sum(download_summary$expected_filings),
    2
  ),
  "%\n"
)

cat("\nOutput files:\n")
cat(log_path, "\n")
cat(summary_path, "\n")
cat(failed_path, "\n")

cat("\nDownload procedure finished.\n")
