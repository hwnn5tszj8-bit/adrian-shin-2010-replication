# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 04: Automated balance sheet extraction
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(tibble)

setwd("~/GitHub/adrian-shin-2010-replication")

# 1. Load downloaded documents

downloads <- read_csv(
  "output/logs/sec_document_download_log.csv",
  show_col_types = FALSE
) %>%
  filter(file.exists(local_file))

# 2. Extract first numeric value from a financial row

extract_number <- function(x) {
  
  x <- as.character(x)
  
  x <- str_trim(x)
  
  x <- gsub(",", "", x, fixed = TRUE)
  
  x <- gsub("$", "", x, fixed = TRUE)
  
  x <- str_trim(x)
  
  valid <- grepl(
    "^[0-9]+(\\.[0-9]+)?$",
    x
  )
  
  values <- suppressWarnings(
    as.numeric(x[valid])
  )
  
  if (length(values) == 0) {
    return(NA_real_)
  }
  
  values[1]
}

# 3. Extract balance sheet candidates

extract_balance_sheet <- function(path) {
  
  document <- read_html(path)
  
  tables <- html_elements(document, "table")
  
  candidates <- list()
  
  for (i in seq_along(tables)) {
    
    tab <- tryCatch(
      html_table(tables[[i]]),
      error = function(e) NULL
    )
    
    if (is.null(tab)) {
      next
    }
    
    if (nrow(tab) == 0 || ncol(tab) < 2) {
      next
    }
    
    labels <- vapply(
      seq_len(nrow(tab)),
      function(j) {
        
        cells <- as.character(
          unlist(tab[j, ], use.names = FALSE)
        )
        
        cells <- cells[
          !is.na(cells) & trimws(cells) != ""
        ]
        
        if (length(cells) == 0) {
          return("")
        }
        
        str_squish(cells[1])
      },
      character(1)
    )
    
    assets_row <- which(
      grepl(
        "^total assets\\s*$",
        labels,
        ignore.case = TRUE
      )
    )
    
    liabilities_row <- which(
      grepl(
        "^total liabilities\\s*$",
        labels,
        ignore.case = TRUE
      )
    )
    
    equity_row <- which(
      grepl(
        "^total (stockholder|shareholder|stockholders|shareholders).*equity",
        labels,
        ignore.case = TRUE
      ) |
        grepl(
          "^total equity\\s*$",
          labels,
          ignore.case = TRUE
        )
    )
    
    if (
      length(assets_row) == 0 ||
      length(liabilities_row) == 0 ||
      length(equity_row) == 0
    ) {
      next
    }
    
    assets <- extract_number(
      unlist(tab[assets_row[1], ])
    )
    
    liabilities <- extract_number(
      unlist(tab[liabilities_row[1], ])
    )
    
    equity <- extract_number(
      unlist(tab[equity_row[1], ])
    )
    
    if (anyNA(c(assets, liabilities, equity))) {
      next
    }
    
    if (assets <= 0 || equity <= 0) {
      next
    }
    
    accounting_error <- abs(
      assets - liabilities - equity
    )
    
    relative_error <- accounting_error / assets
    
    candidates[[length(candidates) + 1]] <- tibble(
      table_number = i,
      total_assets = assets,
      total_liabilities = liabilities,
      book_equity = equity,
      accounting_error = accounting_error,
      relative_error = relative_error,
      leverage = assets / equity
    )
  }
  
  if (length(candidates) == 0) {
    return(tibble())
  }
  
  bind_rows(candidates)
}

# 4. Process downloaded reports

all_candidates <- list()

for (i in seq_len(nrow(downloads))) {
  
  row <- downloads[i, ]
  
  cat(
    "\nProcessing:",
    row$bank,
    row$form,
    as.character(row$report_date),
    "\n"
  )
  
  result <- tryCatch(
    extract_balance_sheet(row$local_file),
    error = function(e) {
      
      cat("ERROR:", conditionMessage(e), "\n")
      
      tibble()
    }
  )
  
  if (nrow(result) == 0) {
    
    cat("No balance sheet candidates found.\n")
    
    next
  }
  
  result <- result %>%
    mutate(
      bank = row$bank,
      form = row$form,
      report_date = row$report_date,
      accession_number = row$accession_number,
      local_file = row$local_file,
      source_url = row$source_url,
      .before = 1
    )
  
  all_candidates[[length(all_candidates) + 1]] <- result
  
  cat("Candidates found:", nrow(result), "\n")
}

# 5. Combine extraction results

if (length(all_candidates) == 0) {
  stop("No balance sheet candidates were extracted.")
}

candidates <- bind_rows(all_candidates)

# 6. Select preliminary balance sheets
# Retain candidates satisfying the accounting identity.
# Largest assets is a heuristic, not final verification.

selected <- candidates %>%
  filter(relative_error <= 0.001) %>%
  group_by(bank, accession_number) %>%
  slice_max(
    order_by = total_assets,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    verified = FALSE,
    source_units = NA_character_,
    extraction_method = "Automated HTML extraction"
  ) %>%
  arrange(bank, report_date)

# 7. Save results

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  candidates,
  "data/processed/balance_sheet_candidates.csv"
)

write_csv(
  selected,
  "data/processed/pilot_balance_sheets.csv"
)

# 8. Display extraction results

cat("\n========================================\n")
cat("BALANCE SHEET EXTRACTION COMPLETE\n")
cat("========================================\n")

cat("Documents available:", nrow(downloads), "\n")
cat("Candidates extracted:", nrow(candidates), "\n")
cat("Balance sheets selected:", nrow(selected), "\n\n")

print(
  selected %>%
    select(
      bank,
      report_date,
      table_number,
      total_assets,
      total_liabilities,
      book_equity,
      leverage
    ),
  n = Inf,
  width = Inf
)

# 9. Identify documents requiring further investigation

missing <- downloads %>%
  anti_join(
    selected,
    by = c("bank", "accession_number")
  )

write_csv(
  missing,
  "output/logs/balance_sheet_extraction_missing.csv"
)

cat("\nDocuments requiring further investigation:\n")

print(
  missing %>%
    select(bank, form, report_date),
  n = Inf
)