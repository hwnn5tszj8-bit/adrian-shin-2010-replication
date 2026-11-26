
# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 07: Historical balance sheet extraction
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(tibble)


# ------------------------------------------------------------
# 1. Configuration
# ------------------------------------------------------------

processed_dir <- "data/processed"
log_dir <- "output/logs"

dir.create(processed_dir, recursive = TRUE,
           showWarnings = FALSE)

dir.create(log_dir, recursive = TRUE,
           showWarnings = FALSE)

downloads <- read_csv(
  "output/logs/full_sec_document_download_log.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  filter(
    status %in% c("cached", "downloaded"),
    !is.na(local_file),
    file.exists(local_file)
  ) %>%
  distinct(accession_number, .keep_all = TRUE)

if (nrow(downloads) == 0L) stop("No cached filings found. Download the SEC documents before running extraction.")

cat("Documents available:", nrow(downloads), "\n")

# ------------------------------------------------------------
# 2. Normalize text
# ------------------------------------------------------------

clean_text <- function(x) {
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- gsub("\u2019", "'", x, fixed = TRUE)
  
  str_squish(x)
}

# ------------------------------------------------------------
# 3. Parse financial numbers
# ------------------------------------------------------------

parse_amount <- function(x) {
  
  x <- clean_text(x)
  
  x <- gsub("$", "", x, fixed = TRUE)
  x <- gsub(",", "", x, fixed = TRUE)
  x <- trimws(x)
  
  x <- sub(
    "^\\((.*)\\)$",
    "-\\1",
    x
  )
  
  valid <- grepl(
    "^-?[0-9]+(\\.[0-9]+)?$",
    x
  )
  
  result <- rep(NA_real_, length(x))
  
  result[valid] <- suppressWarnings(
    as.numeric(x[valid])
  )
  
  result
}

# ------------------------------------------------------------
# 4. Detect monetary units
# ------------------------------------------------------------

detect_units <- function(context) {
  
  context <- tolower(
    paste(context, collapse = " ")
  )
  
  millions <- grepl(
    "(in|expressed in|amounts in)\\s+(u\\.?s\\.?\\s+)?(dollars\\s+in\\s+)?millions|\\$\\s*in\\s+millions|millions of dollars",
    context
  )
  
  thousands <- grepl(
    "(in|expressed in|amounts in)\\s+(u\\.?s\\.?\\s+)?(dollars\\s+in\\s+)?thousands|\\$\\s*in\\s+thousands|thousands of dollars",
    context
  )
  
  if (millions && !thousands) {
    return("USD millions")
  }
  
  if (thousands && !millions) {
    return("USD thousands")
  }
  
  "Unknown"
}

unit_multiplier <- function(units) {
  
  switch(
    units,
    "USD millions" = 1,
    "USD thousands" = 0.001,
    NA_real_
  )
}

# ------------------------------------------------------------
# 5. Identify financial statement rows
# ------------------------------------------------------------

classify_label <- function(x) {
  
  x <- tolower(clean_text(x))
  
  x <- sub(
    "[[:space:][:punct:]]+$",
    "",
    x
  )
  
  if (grepl("^total assets$", x)) {
    return("assets")
  }
  
  if (grepl("^total liabilities$", x)) {
    return("liabilities")
  }
  
  if (grepl(
    "^(total )?(stockholders?|shareholders?|share owners?|stock owners?)'?s? equity$|^total equity$",
    x
  )) {
    return("equity")
  }
  
  if (grepl(
    "^total liabilities and .*equity$",
    x
  )) {
    return("liabilities_and_equity")
  }
  
  "other"
}

# ------------------------------------------------------------
# 6. Construct a candidate observation
# ------------------------------------------------------------

make_candidate <- function(
    assets,
    liabilities,
    equity,
    context,
    column_context,
    method,
    reference,
    column_number,
    column_count
) {
  
  if (
    is.na(assets) ||
    is.na(equity) ||
    assets <= 0 ||
    equity <= 0 ||
    equity > assets
  ) {
    return(NULL)
  }
  
  units <- detect_units(context)
  multiplier <- unit_multiplier(units)
  
  statement_context <- tolower(context)
  column_context_lower <- tolower(column_context)
  
  consolidated <- grepl(
    "consolidated",
    statement_context
  )
  
  parent_only <- grepl(
    "parent company only|parent-only|parent company financial statements|unconsolidated balance sheet|parent company balance sheet",
    statement_context
  )
  
  consolidated_column <- grepl(
    "consolidated",
    column_context_lower
  )
  
  subsidiary_column <- grepl(
    "subsidiar|elimination|parent company",
    column_context_lower
  ) && !consolidated_column
  
  balance_sheet_heading <- grepl(
    "balance sheets?|statements? of financial condition|statements? of financial position",
    statement_context
  )
  
  accounting_error <- if (
    !is.na(liabilities)
  ) {
    assets - liabilities - equity
  } else {
    NA_real_
  }
  
  relative_error <- if (
    !is.na(accounting_error)
  ) {
    abs(accounting_error) / assets
  } else {
    NA_real_
  }
  
  accounting_check <- case_when(
    is.na(liabilities) ~ "Not independently checked",
    relative_error <= 0.001 ~ "Passed",
    TRUE ~ "Failed"
  )
  
  # Heuristic ranking only.
  # No candidate is automatically verified.
  
  score <- 0
  
  if (balance_sheet_heading) score <- score + 3
  if (consolidated) score <- score + 3
  if (consolidated_column) score <- score + 5
  if (parent_only) score <- score - 15
  if (subsidiary_column) score <- score - 10
  
  if (accounting_check == "Passed") {
    score <- score + 4
  }
  
  if (accounting_check == "Failed") {
    score <- score - 8
  }
  
  if (units != "Unknown") {
    score <- score + 2
  }
  
  tibble(
    extraction_method = method,
    source_reference = reference,
    column_number = column_number,
    column_count = column_count,
    
    raw_total_assets = assets,
    raw_total_liabilities = liabilities,
    raw_book_equity = equity,
    
    source_units = units,
    unit_multiplier = multiplier,
    
    total_assets = assets * multiplier,
    total_liabilities = liabilities * multiplier,
    book_equity = equity * multiplier,
    
    implied_liabilities = (assets - equity) *
      multiplier,
    
    leverage = assets / equity,
    
    accounting_error_raw = accounting_error,
    relative_error = relative_error,
    accounting_check = accounting_check,
    
    consolidated_evidence = consolidated,
    consolidated_column = consolidated_column,
    parent_only_evidence = parent_only,
    subsidiary_column = subsidiary_column,
    balance_sheet_heading = balance_sheet_heading,
    
    candidate_score = score,
    column_context = substr(column_context, 1, 1000),
    statement_context = substr(context, 1, 2000),
    
    verified = FALSE
  )
}

# ------------------------------------------------------------
# 7. Extract HTML table candidates
# ------------------------------------------------------------

extract_html_candidates <- function(document) {
  
  tables <- html_elements(document, "table")
  
  candidates <- list()
  
  for (i in seq_along(tables)) {
    
    node <- tables[[i]]
    
    # Skip outer layout tables containing nested tables.
    
    if (length(html_elements(node, "table")) > 0) {
      next
    }
    
    tab <- tryCatch(
      html_table(node),
      error = function(e) NULL
    )
    
    if (
      is.null(tab) ||
      nrow(tab) < 3 ||
      ncol(tab) < 2
    ) {
      next
    }
    
    mat <- as.matrix(tab)
    
    labels <- apply(
      mat,
      1,
      function(row) {
        
        row <- clean_text(row)
        row <- row[nzchar(row)]
        
        if (length(row) == 0) {
          return("")
        }
        
        row[1]
      }
    )
    
    types <- vapply(
      labels,
      classify_label,
      character(1)
    )
    
    asset_rows <- which(types == "assets")
    equity_rows <- which(types == "equity")
    liability_rows <- which(types == "liabilities")
    
    if (
      length(asset_rows) == 0 ||
      length(equity_rows) == 0
    ) {
      next
    }
    
    preceding_nodes <- xml2::xml_find_all(
      node,
      "preceding-sibling::*[position() <= 6]"
    )
    
    preceding_text <- paste(
      html_text2(preceding_nodes),
      collapse = " "
    )
    
    for (a in asset_rows) {
      
      possible_equity <- equity_rows[
        equity_rows > a &
          equity_rows <= a + 100
      ]
      
      if (length(possible_equity) == 0) {
        next
      }
      
      e <- possible_equity[1]
      
      possible_liabilities <- liability_rows[
        liability_rows > a &
          liability_rows < e
      ]
      
      l <- if (length(possible_liabilities)) {
        possible_liabilities[1]
      } else {
        NA_integer_
      }
      
      header_rows <- seq_len(
        min(8, max(1, a - 1))
      )
      
      context <- paste(
        preceding_text,
        paste(mat[header_rows, , drop = FALSE],
              collapse = " "),
        paste(labels[seq_len(e)], collapse = " ")
      )
      
      assets_values <- parse_amount(mat[a, ])
      equity_values <- parse_amount(mat[e, ])
      
      liabilities_values <- if (!is.na(l)) {
        parse_amount(mat[l, ])
      } else {
        rep(NA_real_, ncol(mat))
      }
      
      numeric_columns <- which(
        !is.na(assets_values) &
          !is.na(equity_values)
      )
      
      for (j in numeric_columns) {
        
        column_context <- paste(
          clean_text(mat[header_rows, j]),
          collapse = " "
        )
        
        candidate <- make_candidate(
          assets = assets_values[j],
          liabilities = liabilities_values[j],
          equity = equity_values[j],
          context = context,
          column_context = column_context,
          method = "HTML table",
          reference = paste0("HTML table ", i),
          column_number = j,
          column_count = length(numeric_columns)
        )
        
        if (!is.null(candidate)) {
          
          candidates[[length(candidates) + 1]] <-
            candidate
        }
      }
    }
  }
  
  bind_rows(candidates)
}

# ------------------------------------------------------------
# 8. Extract numeric values from plain-text rows
# ------------------------------------------------------------

extract_text_amounts <- function(line) {
  
  line <- gsub(
    "<[^>]+>",
    " ",
    line
  )
  
  matches <- str_extract_all(
    line,
    "\\(?-?[0-9][0-9,]*(?:\\.[0-9]+)?\\)?"
  )[[1]]
  
  parse_amount(matches)
}

# ------------------------------------------------------------
# 9. Extract plain-text balance sheet candidates
# ------------------------------------------------------------

extract_text_candidates <- function(lines) {
  
  lines <- gsub(
    "<[^>]+>",
    " ",
    lines
  )
  
  normalized <- tolower(
    clean_text(lines)
  )
  
  asset_rows <- which(
    grepl(
      "^total assets\\b",
      normalized
    )
  )
  
  liability_rows <- which(
    grepl(
      "^total liabilities\\b",
      normalized
    ) &
      !grepl(
        "^total liabilities and",
        normalized
      )
  )
  
  equity_rows <- which(
    grepl(
      "^(total )?(stockholders?|shareholders?)'?s? equity\\b|^total equity\\b",
      normalized
    )
  )
  
  candidates <- list()
  
  for (a in asset_rows) {
    
    possible_equity <- equity_rows[
      equity_rows > a &
        equity_rows <= a + 120
    ]
    
    if (length(possible_equity) == 0) {
      next
    }
    
    e <- possible_equity[1]
    
    possible_liabilities <- liability_rows[
      liability_rows > a &
        liability_rows < e
    ]
    
    l <- if (length(possible_liabilities)) {
      possible_liabilities[1]
    } else {
      NA_integer_
    }
    
    assets_values <- extract_text_amounts(
      lines[a]
    )
    
    equity_values <- extract_text_amounts(
      lines[e]
    )
    
    # Only align rows containing the same number of
    # numerical values. Otherwise require review.
    
    if (
      length(assets_values) == 0 ||
      length(assets_values) != length(equity_values)
    ) {
      next
    }
    
    liabilities_values <- if (!is.na(l)) {
      extract_text_amounts(lines[l])
    } else {
      numeric()
    }
    
    if (
      length(liabilities_values) !=
      length(assets_values)
    ) {
      liabilities_values <- rep(
        NA_real_,
        length(assets_values)
      )
    }
    
    context_start <- max(1, a - 45)
    
    context <- paste(
      lines[context_start:e],
      collapse = " "
    )
    
    for (j in seq_along(assets_values)) {
      
      candidate <- make_candidate(
        assets = assets_values[j],
        liabilities = liabilities_values[j],
        equity = equity_values[j],
        context = context,
        column_context = "",
        method = "Plain text",
        reference = paste0(
          "Lines ", a, "-", e
        ),
        column_number = j,
        column_count = length(assets_values)
      )
      
      if (!is.null(candidate)) {
        
        candidates[[length(candidates) + 1]] <-
          candidate
      }
    }
  }
  
  bind_rows(candidates)
}

# ------------------------------------------------------------
# 10. Process one historical document
# ------------------------------------------------------------

extract_document <- function(path) {
  
  lines <- readLines(
    path,
    warn = FALSE,
    skipNul = TRUE
  )
  
  # Convert legacy encodings where necessary.
  
  lines <- iconv(
    lines,
    from = "",
    to = "UTF-8",
    sub = " "
  )
  
  html_candidates <- tibble()
  text_candidates <- tibble()
  
  contains_tables <- any(
    grepl(
      "<table\\b",
      lines,
      ignore.case = TRUE
    )
  )
  
  if (contains_tables) {
    
    document <- tryCatch(
      read_html(path),
      error = function(e) NULL
    )
    
    if (!is.null(document)) {
      
      html_candidates <- extract_html_candidates(
        document
      )
    }
  }
  
  # Historical SGML and plain-text filings.
  # Also inspect HTML PRE blocks where available.
  
  if (
    !contains_tables ||
    nrow(html_candidates) == 0 ||
    grepl("\\.txt$", path, ignore.case = TRUE)
  ) {
    
    text_candidates <- extract_text_candidates(
      lines
    )
  }
  
  bind_rows(
    html_candidates,
    text_candidates
  )
}

# ------------------------------------------------------------
# 11. Process all historical filings
# ------------------------------------------------------------

all_candidates <- list()
processing_log <- list()

cat("\nStarting historical extraction...\n\n")

for (i in seq_len(nrow(downloads))) {
  
  row <- downloads[i, ]
  
  error_message <- ""
  
  result <- tryCatch(
    
    extract_document(row$local_file),
    
    error = function(e) {
      
      error_message <<- conditionMessage(e)
      
      tibble()
    }
  )
  
  if (nrow(result) > 0) {
    
    result <- result %>%
      mutate(
        bank = row$bank,
        form = row$form,
        
        # This is the filing's reporting date,
        # not yet the confirmed column date.
        
        report_date = row$report_date,
        accession_number = row$accession_number,
        local_file = row$local_file,
        source_url = row$source_url,
        .before = 1
      )
    
    all_candidates[[length(all_candidates) + 1]] <-
      result
  }
  
  processing_log[[i]] <- tibble(
    bank = row$bank,
    form = row$form,
    report_date = row$report_date,
    accession_number = row$accession_number,
    local_file = row$local_file,
    candidates_found = nrow(result),
    error = error_message
  )
  
  cat(
    sprintf(
      "[%03d/%03d] %-16s %-10s candidates: %d\n",
      i,
      nrow(downloads),
      row$bank,
      ifelse(
        is.na(row$report_date),
        "Unknown",
        row$report_date
      ),
      nrow(result)
    )
  )
}

processing_log <- bind_rows(processing_log)

write_csv(
  processing_log,
  file.path(
    log_dir,
    "historical_extraction_processing_log.csv"
  ),
  na = ""
)

candidates <- bind_rows(all_candidates)

if (nrow(candidates) == 0) {
  stop("No historical candidates extracted.")
}

# ------------------------------------------------------------
# 12. Rank candidates within each filing
# ------------------------------------------------------------

candidates <- candidates %>%
  distinct() %>%
  group_by(bank, accession_number) %>%
  arrange(
    desc(candidate_score),
    .by_group = TRUE
  ) %>%
  mutate(
    candidate_rank = row_number(),
    candidates_in_filing = n(),
    top_score = max(candidate_score),
    
    tied_top_candidates = sum(
      candidate_score == top_score
    )
  ) %>%
  ungroup() %>%
  mutate(
    candidate_id = row_number()
  )

# ------------------------------------------------------------
# 13. Select provisional candidates
# ------------------------------------------------------------

selected <- candidates %>%
  filter(candidate_rank == 1) %>%
  mutate(
    
    confidence = case_when(
      
      parent_only_evidence |
        subsidiary_column ~ "Low",
      
      accounting_check == "Failed" ~ "Low",
      
      source_units == "Unknown" ~ "Low",
      
      tied_top_candidates > 1 ~ "Ambiguous",
      
      consolidated_evidence &
        accounting_check == "Passed" &
        candidate_score >= 9 ~ "Promising",
      
      TRUE ~ "Needs review"
    ),
    
    # Reporting-period identification has not
    # yet been independently validated.
    
    period_verified = FALSE,
    
    # All observations remain provisional.
    
    verified = FALSE,
    review_required = TRUE
  ) %>%
  arrange(bank, report_date)

# ------------------------------------------------------------
# 14. Compare against reconciled pilot observations
# ------------------------------------------------------------

pilot_path <- file.path(
  processed_dir,
  "pilot_balance_sheets_reconciled.csv"
)

pilot <- read_csv(
  pilot_path,
  show_col_types = FALSE
) %>%
  transmute(
    bank,
    report_date = as.character(report_date),
    pilot_assets = total_assets,
    pilot_equity = book_equity
  )

pilot_comparison <- pilot %>%
  left_join(
    selected %>%
      select(
        bank,
        report_date,
        accession_number,
        total_assets,
        book_equity,
        source_units,
        confidence,
        source_reference,
        column_number
      ),
    by = c("bank", "report_date")
  ) %>%
  mutate(
    
    assets_difference = total_assets -
      pilot_assets,
    
    equity_difference = book_equity -
      pilot_equity,
    
    pilot_match = case_when(
      
      is.na(total_assets) |
        is.na(book_equity) ~ "Not extracted",
      
      abs(assets_difference) <= 1 &
        abs(equity_difference) <= 1 ~ "Match",
      
      TRUE ~ "Mismatch"
    )
  )

# Check whether a matching candidate exists
# even when it was not selected as rank 1.

pilot_candidate_matches <- candidates %>%
  inner_join(
    pilot,
    by = c("bank", "report_date")
  ) %>%
  filter(
    !is.na(total_assets),
    !is.na(book_equity),
    abs(total_assets - pilot_assets) <= 1,
    abs(book_equity - pilot_equity) <= 1
  )

# ------------------------------------------------------------
# 15. Construct review queue
# ------------------------------------------------------------

review_queue <- downloads %>%
  select(
    bank,
    form,
    report_date,
    accession_number,
    local_file,
    source_url
  ) %>%
  left_join(
    selected %>%
      select(
        accession_number,
        candidate_id,
        source_reference,
        column_number,
        total_assets,
        total_liabilities,
        book_equity,
        leverage,
        source_units,
        confidence,
        accounting_check,
        tied_top_candidates
      ),
    by = "accession_number"
  ) %>%
  mutate(
    
    review_reason = case_when(
      
      is.na(candidate_id) ~ "No candidate extracted",
      
      source_units == "Unknown" ~ "Unknown monetary units",
      
      confidence == "Low" ~ "Weak statement evidence",
      
      tied_top_candidates > 1 ~ "Ambiguous candidates",
      
      accounting_check == "Failed" ~ "Accounting discrepancy",
      
      TRUE ~ "Confirm statement and reporting period"
    )
  )

# ------------------------------------------------------------
# 16. Extraction summary by bank
# ------------------------------------------------------------

extraction_summary <- processing_log %>%
  group_by(bank) %>%
  summarise(
    
    documents = n(),
    
    documents_with_candidates = sum(
      candidates_found > 0
    ),
    
    documents_without_candidates = sum(
      candidates_found == 0
    ),
    
    total_candidates = sum(
      candidates_found
    ),
    
    extraction_pct = round(
      100 * documents_with_candidates / documents,
      2
    ),
    
    .groups = "drop"
  )

quality_summary <- selected %>%
  group_by(bank) %>%
  summarise(
    
    known_units = sum(
      source_units != "Unknown"
    ),
    
    accounting_passed = sum(
      accounting_check == "Passed"
    ),
    
    promising = sum(
      confidence == "Promising"
    ),
    
    ambiguous = sum(
      confidence == "Ambiguous"
    ),
    
    verified = sum(verified),
    
    .groups = "drop"
  )

extraction_summary <- extraction_summary %>%
  left_join(
    quality_summary,
    by = "bank"
  ) %>%
  mutate(
    across(
      c(
        known_units,
        accounting_passed,
        promising,
        ambiguous,
        verified
      ),
      ~ coalesce(.x, 0L)
    )
  )

# ------------------------------------------------------------
# 17. Save all outputs
# ------------------------------------------------------------

write_csv(
  candidates,
  file.path(
    processed_dir,
    "historical_balance_sheet_candidates.csv"
  ),
  na = ""
)

write_csv(
  selected,
  file.path(
    processed_dir,
    "historical_balance_sheets.csv"
  ),
  na = ""
)

write_csv(
  review_queue,
  file.path(
    log_dir,
    "historical_balance_sheet_review.csv"
  ),
  na = ""
)

write_csv(
  extraction_summary,
  file.path(
    log_dir,
    "historical_extraction_summary.csv"
  ),
  na = ""
)

write_csv(
  pilot_comparison,
  file.path(
    log_dir,
    "historical_pilot_comparison.csv"
  ),
  na = ""
)

write_csv(
  pilot_candidate_matches,
  file.path(
    log_dir,
    "historical_pilot_candidate_matches.csv"
  ),
  na = ""
)

# ------------------------------------------------------------
# 18. Display concise results
# ------------------------------------------------------------

cat("\n========================================\n")
cat("HISTORICAL BALANCE SHEET EXTRACTION\n")
cat("========================================\n\n")

print(
  extraction_summary,
  n = Inf,
  width = Inf
)

cat("\nDocuments processed:",
    nrow(downloads), "\n")

cat("Total candidates:",
    nrow(candidates), "\n")

cat("Provisional selections:",
    nrow(selected), "\n")

cat("Verified observations:",
    sum(selected$verified), "\n")

cat("\nPilot benchmark comparison:\n\n")

print(
  pilot_comparison %>%
    select(
      bank,
      report_date,
      pilot_assets,
      total_assets,
      pilot_equity,
      book_equity,
      pilot_match
    ),
  n = Inf,
  width = Inf
)

cat("\nPilot observations with matching candidates:\n")

print(
  pilot_candidate_matches %>%
    distinct(bank, report_date) %>%
    count(bank),
  n = Inf
)

cat("\nHistorical extraction completed.\n")
