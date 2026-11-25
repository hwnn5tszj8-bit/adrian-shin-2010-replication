# ==============================================================================
# Script 01: Build SEC filing inventory for the original five-bank panel
# Adrian & Shin (2010), "Liquidity and Leverage", Table 3
#
# Downloads filing metadata only. It does not assume companyfacts contains
# every historical balance-sheet, repo, or VaR item. Those require filing-level
# extraction and a documented mapping.
# ==============================================================================

required_packages <- c("httr2", "jsonlite", "dplyr", "purrr", "readr", "stringr", "tibble", "lubridate")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) install.packages(missing_packages)

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)

# SEC requires a descriptive User-Agent with a working email address.
# Add SEC_USER_AGENT="Your Name your.email@university.edu" to ~/.Renviron.
# Do not commit your email address or credentials.
sec_user_agent <- Sys.getenv("SEC_USER_AGENT")
if (identical(sec_user_agent, "")) {
  stop("Set SEC_USER_AGENT in your .Renviron before running this script.")
}

dir.create("data/raw/sec_submissions", recursive = TRUE, showWarnings = FALSE)
dir.create("data/raw/sec_filings", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/logs", recursive = TRUE, showWarnings = FALSE)

# Bank-specific sample windows reported in Table 1 of the paper.
banks <- tribble(
  ~bank, ~cik, ~sample_start, ~sample_end,
  "Bear Stearns",    "0000777001", "1997-03-31", "2008-03-31",
  "Goldman Sachs",   "0000886982", "1999-06-30", "2008-03-31",
  "Lehman Brothers", "0000806085", "1993-06-30", "2008-03-31",
  "Merrill Lynch",   "0000065100", "1991-03-31", "2008-03-31",
  "Morgan Stanley",  "0000895421", "1997-06-30", "2008-03-31"
) %>%
  mutate(
    sample_start = as.Date(sample_start),
    sample_end = as.Date(sample_end),
    cik_numeric = as.character(as.integer(cik))
  )

# SEC fair-access guidance: remain comfortably below 10 requests per second.
sec_get_json <- function(url) {
  response <- httr2::request(url) %>%
    httr2::req_user_agent(sec_user_agent) %>%
    httr2::req_headers(Accept = "application/json") %>%
    httr2::req_throttle(rate = 8, capacity = 8) %>%
    httr2::req_perform()
  httr2::resp_body_json(response, simplifyVector = FALSE)
}

# SEC submissions JSON stores filing metadata in a column-oriented structure.
recent_to_tibble <- function(recent, bank, cik) {
  if (is.null(recent) || length(recent) == 0) return(tibble())
  n <- length(recent[[1]])
  values <- lapply(recent, function(x) {
    if (length(x) == n) unlist(x, use.names = FALSE) else rep(NA_character_, n)
  })
  as_tibble(values) %>% mutate(bank = bank, cik = cik, .before = 1)
}

all_filings <- vector("list", nrow(banks))

for (i in seq_len(nrow(banks))) {
  bank_i <- banks$bank[[i]]
  cik_i <- banks$cik[[i]]
  message("Retrieving SEC submissions history: ", bank_i, " (CIK ", cik_i, ")")

  submissions_url <- paste0("https://data.sec.gov/submissions/CIK", cik_i, ".json")
  submissions <- sec_get_json(submissions_url)

  jsonlite::write_json(
    submissions,
    file = file.path("data/raw/sec_submissions", paste0(cik_i, ".json")),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  pieces <- list(recent_to_tibble(submissions$filings$recent, bank_i, cik_i))

  # Older filing metadata can be stored in additional SEC submissions JSON files.
  old_files <- submissions$filings$files
  if (!is.null(old_files) && length(old_files) > 0) {
    for (j in seq_along(old_files)) {
      old_name <- old_files[[j]]$name
      old_url <- paste0("https://data.sec.gov/submissions/", old_name)
      message("  Downloading historical index: ", old_name)
      old_json <- sec_get_json(old_url)
      jsonlite::write_json(
        old_json,
        file = file.path("data/raw/sec_submissions", old_name),
        auto_unbox = TRUE,
        pretty = TRUE
      )
      pieces[[length(pieces) + 1]] <- recent_to_tibble(
        old_json$filings$recent, bank_i, cik_i
      )
    }
  }

  all_filings[[i]] <- bind_rows(pieces)
}

filings <- bind_rows(all_filings) %>%
  mutate(
    filingDate = as.Date(filingDate),
    reportDate = as.Date(reportDate),
    form = as.character(form),
    accessionNumber = as.character(accessionNumber),
    primaryDocument = as.character(primaryDocument)
  ) %>%
  filter(form %in% c("10-K", "10-Q", "10-K/A", "10-Q/A"), !is.na(reportDate)) %>%
  left_join(
    banks %>% select(bank, sample_start, sample_end, cik_numeric),
    by = c("bank", "cik_numeric")
  ) %>%
  filter(reportDate >= sample_start, reportDate <= sample_end) %>%
  mutate(
    accession_path = str_replace_all(accessionNumber, "-", ""),
    filing_url = if_else(
      !is.na(primaryDocument) & primaryDocument != "",
      paste0(
        "https://www.sec.gov/Archives/edgar/data/",
        as.integer(cik), "/", accession_path, "/", primaryDocument
      ),
      NA_character_
    ),
    report_quarter = paste0(
      lubridate::year(reportDate), "Q", lubridate::quarter(reportDate)
    )
  ) %>%
  arrange(bank, reportDate, filingDate)

write_csv(
  filings,
  "data/raw/sec_filings/original_five_bank_filing_inventory.csv",
  na = ""
)

coverage <- filings %>%
  count(bank, report_quarter, name = "n_filings") %>%
  arrange(bank, report_quarter)

write_csv(coverage, "output/logs/sec_filing_coverage_by_bank_quarter.csv", na = "")

message("SEC filing inventory created: ", nrow(filings), " filing records.")
