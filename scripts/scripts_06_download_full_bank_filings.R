
# ============================================================
# 06_download_full_bank_filings.R
#
# Adrian & Shin (2010) - Liquidity and Leverage
# Full historical SEC filing download
#
# Purpose:
#   Download original 10-K and 10-Q filings
#   Preserve existing documents
#   Retry temporary HTTP failures
#   Maintain resumable download logs
# ============================================================

library(httr)
library(readr)
library(dplyr)
library(stringr)

# ------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------

inventory_path <- file.path(
  "data", "raw", "sec_filings",
  "bank_financial_extraction.csv"
)

document_dir <- file.path(
  "data", "raw", "sec_filings", "documents"
)

log_dir <- file.path("output", "logs")

log_path <- file.path(
  log_dir, "full_sec_document_download_log.csv"
)

failed_path <- file.path(
  log_dir, "full_sec_document_download_failed.csv"
)

summary_path <- file.path(
  log_dir, "full_sec_document_download_summary.csv"
)

dir.create(document_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

sec_user_agent <- Sys.getenv("SEC_USER_AGENT")

if (!nzchar(sec_user_agent)) {
  stop("SEC_USER_AGENT is not available in this R session.")
}

if (!file.exists(inventory_path)) {
  stop("Filing inventory not found: ", inventory_path)
}

max_attempts <- 4L
request_delay <- 0.5
request_timeout <- 90

# ------------------------------------------------------------
# 2. Read the existing filing inventory
# ------------------------------------------------------------

inventory <- read_csv(
  inventory_path,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

# Support common column names used in earlier scripts.

find_column <- function(data, alternatives, required = TRUE) {
  
  matches <- alternatives[alternatives %in% names(data)]
  
  if (length(matches) > 0) {
    return(matches[1])
  }
  
  if (required) {
    stop(
      "Required inventory column not found.\n",
      "Expected one of: ",
      paste(alternatives, collapse = ", "),
      "\nAvailable columns: ",
      paste(names(data), collapse = ", ")
    )
  }
  
  return(NULL)
}

bank_col <- find_column(
  inventory,
  c("bank", "bank_name", "company", "company_name")
)

form_col <- find_column(
  inventory,
  c("form", "form_type", "filing_type")
)

accession_col <- find_column(
  inventory,
  c("accession_number", "accessionNumber", "accession")
)

date_col <- find_column(
  inventory,
  c("report_date", "reportDate", "period_end", "report_period"),
  required = FALSE
)

url_col <- find_column(
  inventory,
  c(
    "document_url",
    "primary_document_url",
    "primary_doc_url",
    "filing_url",
    "sec_url",
    "url"
  ),
  required = FALSE
)

path_col <- find_column(
  inventory,
  c("local_path", "file_path", "document_path", "local_file"),
  required = FALSE
)

# ------------------------------------------------------------
# 3. Standardize the inventory
# ------------------------------------------------------------

filings <- tibble(
  bank = inventory[[bank_col]],
  form = inventory[[form_col]],
  accession_number = inventory[[accession_col]],
  report_date = if (!is.null(date_col)) {
    inventory[[date_col]]
  } else {
    rep(NA_character_, nrow(inventory))
  },
  document_url = if (!is.null(url_col)) {
    inventory[[url_col]]
  } else {
    rep(NA_character_, nrow(inventory))
  },
  existing_path = if (!is.null(path_col)) {
    inventory[[path_col]]
  } else {
    rep(NA_character_, nrow(inventory))
  }
)

# Retain original reports, excluding amendments.

filings <- filings %>%
  mutate(
    form = str_to_upper(str_trim(form)),
    accession_number = str_trim(accession_number),
    accession_clean = str_remove_all(accession_number, "-")
  ) %>%
  filter(form %in% c("10-K", "10-Q"))

if (any(
  is.na(filings$accession_clean) |
  !str_detect(filings$accession_clean, "^[0-9]{18}$")
)) {
  stop("Missing or invalid accession numbers in inventory.")
}

filings <- filings %>%
  distinct(accession_clean, .keep_all = TRUE)

if (nrow(filings) == 0) {
  stop("No original 10-K or 10-Q filings found.")
}

# ------------------------------------------------------------
# 4. Construct missing SEC document URLs
# ------------------------------------------------------------

needs_url <- is.na(filings$document_url) |
  !nzchar(filings$document_url)

if (any(needs_url)) {
  
  cik_col <- find_column(
    inventory,
    c("cik", "CIK", "company_cik"),
    required = FALSE
  )
  
  primary_col <- find_column(
    inventory,
    c("primary_document", "primaryDocument", "primary_doc"),
    required = FALSE
  )
  
  if (is.null(cik_col) || is.null(primary_col)) {
    stop(
      "Some filing URLs are missing and cannot be constructed ",
      "from the available inventory columns."
    )
  }
  
  original_rows <- match(
    filings$accession_clean,
    str_remove_all(inventory[[accession_col]], "-")
  )
  
  cik <- str_remove(
    inventory[[cik_col]][original_rows],
    "^0+"
  )
  
  primary_document <- inventory[[primary_col]][original_rows]
  
  filings$document_url[needs_url] <- paste0(
    "https://www.sec.gov/Archives/edgar/data/",
    cik[needs_url], "/",
    filings$accession_clean[needs_url], "/",
    primary_document[needs_url]
  )
}

valid_urls <- !is.na(filings$document_url) &
  str_detect(
    filings$document_url,
    "^https://www\\.sec\\.gov/Archives/edgar/data/"
  )

if (any(!valid_urls)) {
  stop("Invalid or unsupported SEC document URLs in inventory.")
}

filings <- filings %>%
  arrange(bank, report_date, accession_number)

cat("\nHistorical filings selected:", nrow(filings), "\n\n")

print(
  filings %>%
    count(bank, form, name = "filings"),
  n = Inf
)

if (nrow(filings) != 222L) {
  warning(
    "Expected 222 original reports from the previous inventory. ",
    "Current inventory contains ", nrow(filings), "."
  )
}

# ------------------------------------------------------------
# 5. Initialize persistent download log
# ------------------------------------------------------------

empty_log <- tibble(
  bank = character(),
  form = character(),
  report_date = character(),
  accession_number = character(),
  accession_clean = character(),
  document_url = character(),
  local_path = character(),
  status = character(),
  http_status = integer(),
  attempts = integer(),
  bytes = double(),
  message = character(),
  checked_at = character()
)

download_log <- empty_log

if (file.exists(log_path)) {
  
  download_log <- read_csv(
    log_path,
    col_types = cols(
      .default = col_character(),
      http_status = col_integer(),
      attempts = col_integer(),
      bytes = col_double()
    ),
    show_col_types = FALSE
  )
  
  if (!all(names(empty_log) %in% names(download_log))) {
    stop("Existing download log has an unexpected structure.")
  }
}

# ------------------------------------------------------------
# 6. Helper functions
# ------------------------------------------------------------

safe_filename <- function(x) {
  str_replace_all(x, "[^A-Za-z0-9_-]", "_")
}

valid_document <- function(path) {
  
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    return(FALSE)
  }
  
  size <- file.info(path)$size
  
  if (is.na(size) || size < 1000) {
    return(FALSE)
  }
  
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  
  header <- readBin(
    con,
    what = "raw",
    n = 16384L
  )
  
  # Remove null bytes before inspecting the document header.
  
  header <- header[header != as.raw(0)]
  
  header <- tolower(rawToChar(header))
  
  error_page <- grepl(
    paste(
      "your request originates from an undeclared automated tool",
      "request rate threshold exceeded",
      "<title>access denied",
      "<title>forbidden",
      "<title>404",
      "<title>503",
      "<title>504",
      "<title>service unavailable",
      "<title>gateway timeout",
      sep = "|"
    ),
    header
  )
  
  return(!error_page)
}

make_log_row <- function(
    filing,
    path,
    status,
    http_status = NA_integer_,
    attempts = 0L,
    message = ""
) {
  
  tibble(
    bank = filing$bank,
    form = filing$form,
    report_date = filing$report_date,
    accession_number = filing$accession_number,
    accession_clean = filing$accession_clean,
    document_url = filing$document_url,
    local_path = path,
    status = status,
    http_status = as.integer(http_status),
    attempts = as.integer(attempts),
    bytes = if (!is.na(path) && file.exists(path)) {
      as.numeric(file.info(path)$size)
    } else {
      NA_real_
    },
    message = message,
    checked_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    )
  )
}

# ------------------------------------------------------------
# 7. Identify existing cached documents
# ------------------------------------------------------------

cached_files <- list.files(
  document_dir,
  recursive = TRUE,
  full.names = TRUE
)

cached_files <- cached_files[
  !str_detect(cached_files, "\\.part$")
]

find_cached_document <- function(filing, target_path) {
  
  # First check the exact intended destination.
  
  if (valid_document(target_path)) {
    return(target_path)
  }
  
  # Check paths recorded in the persistent download log.
  
  previous <- download_log %>%
    filter(accession_clean == filing$accession_clean)
  
  candidates <- c(
    previous$local_path,
    filing$existing_path
  )
  
  # Search earlier downloads using the unique accession number.
  
  matching_files <- cached_files[
    str_detect(
      str_remove_all(cached_files, "-"),
      fixed(filing$accession_clean)
    )
  ]
  
  candidates <- unique(c(candidates, matching_files))
  
  candidates <- candidates[
    !is.na(candidates) & nzchar(candidates)
  ]
  
  for (candidate in candidates) {
    
    if (valid_document(candidate)) {
      return(candidate)
    }
  }
  
  return(NA_character_)
}

# ------------------------------------------------------------
# 8. Download one filing with retry handling
# ------------------------------------------------------------

download_filing <- function(filing) {
  
  extension <- tools::file_ext(
    basename(filing$document_url)
  )
  
  if (!nzchar(extension)) {
    extension <- "txt"
  }
  
  bank_dir <- file.path(
    document_dir,
    safe_filename(filing$bank)
  )
  
  dir.create(
    bank_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  target_path <- file.path(
    bank_dir,
    paste0(
      filing$accession_clean,
      ".",
      extension
    )
  )
  
  cached_path <- find_cached_document(
    filing,
    target_path
  )
  
  if (!is.na(cached_path)) {
    
    return(
      make_log_row(
        filing,
        cached_path,
        status = "cached",
        message = "Existing document reused"
      )
    )
  }
  
  temporary_path <- paste0(target_path, ".part")
  
  last_status <- NA_integer_
  last_message <- ""
  attempt <- 0L
  
  for (attempt in seq_len(max_attempts)) {
    
    if (file.exists(temporary_path)) {
      unlink(temporary_path)
    }
    
    Sys.sleep(request_delay)
    
    response <- tryCatch(
      
      GET(
        filing$document_url,
        user_agent(sec_user_agent),
        timeout(request_timeout),
        write_disk(
          temporary_path,
          overwrite = TRUE
        )
      ),
      
      error = function(e) e
    )
    
    if (inherits(response, "error")) {
      
      last_status <- NA_integer_
      last_message <- conditionMessage(response)
      
    } else {
      
      last_status <- status_code(response)
      
      if (
        last_status == 200L &&
        valid_document(temporary_path)
      ) {
        
        if (file.exists(target_path)) {
          unlink(target_path)
        }
        
        moved <- file.rename(
          temporary_path,
          target_path
        )
        
        if (!moved) {
          
          moved <- file.copy(
            temporary_path,
            target_path,
            overwrite = TRUE
          )
          
          if (moved) {
            unlink(temporary_path)
          }
        }
        
        if (moved && valid_document(target_path)) {
          
          return(
            make_log_row(
              filing,
              target_path,
              status = "downloaded",
              http_status = last_status,
              attempts = attempt,
              message = "Download successful"
            )
          )
        }
        
        last_message <- "Unable to save downloaded document"
        
      } else {
        
        last_message <- paste(
          "HTTP", last_status
        )
        
        if (last_status == 200L) {
          last_message <- "Invalid or incomplete document"
        }
      }
      
      # Do not repeatedly request unavailable documents.
      
      if (last_status %in% c(400L, 401L, 404L, 410L)) {
        break
      }
      
      # Stop on access restrictions rather than repeatedly
      # sending requests to the SEC.
      
      if (last_status == 403L) {
        break
      }
    }
    
    if (attempt < max_attempts) {
      
      wait_seconds <- min(
        60,
        5 * 2^(attempt - 1)
      )
      
      if (!inherits(response, "error")) {
        
        retry_after <- suppressWarnings(
          as.numeric(headers(response)[["retry-after"]])
        )
        
        if (
          length(retry_after) == 1L &&
          is.finite(retry_after)
        ) {
          wait_seconds <- max(wait_seconds, retry_after)
        }
      }
      
      Sys.sleep(wait_seconds)
    }
  }
  
  if (file.exists(temporary_path)) {
    unlink(temporary_path)
  }
  
  make_log_row(
    filing,
    target_path,
    status = "failed",
    http_status = last_status,
    attempts = attempt,
    message = last_message
  )
}

# ------------------------------------------------------------
# 9. Execute full historical download
# ------------------------------------------------------------

total_filings <- nrow(filings)

cat("\nStarting full historical download...\n\n")

for (i in seq_len(total_filings)) {
  
  filing <- filings[i, ]
  
  result <- download_filing(filing)
  
  # Keep the latest download status for each accession.
  
  download_log <- download_log %>%
    filter(accession_clean != filing$accession_clean) %>%
    bind_rows(result)
  
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
      filing$bank,
      filing$form,
      ifelse(
        is.na(filing$report_date),
        "Unknown",
        filing$report_date
      ),
      result$status
    )
  )
  
  # Preserve progress and stop if access is restricted.
  
  if (
    result$status == "failed" &&
    !is.na(result$http_status) &&
    result$http_status %in% c(403L, 429L)
  ) {
    
    warning(
      "SEC access restriction detected. ",
      "Progress has been saved. Resume later."
    )
    
    break
  }
}

# ------------------------------------------------------------
# 10. Produce download coverage reports
# ------------------------------------------------------------

current_log <- filings %>%
  select(
    bank,
    form,
    report_date,
    accession_number,
    accession_clean,
    document_url
  ) %>%
  left_join(
    download_log %>%
      select(
        accession_clean,
        local_path,
        status,
        http_status,
        attempts,
        bytes,
        message,
        checked_at
      ),
    by = "accession_clean"
  ) %>%
  mutate(
    status = coalesce(status, "pending")
  )

# Confirm that previously successful files still exist.

available <- vapply(
  current_log$local_path,
  valid_document,
  logical(1)
)

current_log <- current_log %>%
  mutate(
    status = if_else(
      status %in% c("cached", "downloaded") & !available,
      "failed",
      status
    )
  )

download_summary <- current_log %>%
  group_by(bank) %>%
  summarise(
    expected_filings = n(),
    cached = sum(status == "cached"),
    downloaded = sum(status == "downloaded"),
    failed = sum(status == "failed"),
    pending = sum(status == "pending"),
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
  filter(status %in% c("failed", "pending"))

write_csv(
  download_summary,
  summary_path,
  na = ""
)

write_csv(
  failed_filings,
  failed_path,
  na = ""
)

# ------------------------------------------------------------
# 11. Final results
# ------------------------------------------------------------

cat("\n========================================\n")
cat("FULL SEC DOWNLOAD SUMMARY\n")
cat("========================================\n\n")

print(download_summary, n = Inf, width = Inf)

cat(
  "\nTotal expected filings:",
  sum(download_summary$expected_filings),
  "\n"
)

cat(
  "Total available filings:",
  sum(download_summary$available),
  "\n"
)

cat(
  "Failed filings:",
  sum(download_summary$failed),
  "\n"
)

cat(
  "Pending filings:",
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
