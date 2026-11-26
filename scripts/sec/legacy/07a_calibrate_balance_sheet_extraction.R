
# ============================================================
# Adrian & Shin (2010) - Liquidity and Leverage
# Script 07A: Pilot balance sheet extraction calibration
# ============================================================

library(rvest)
library(dplyr)
library(readr)
library(stringr)
library(tibble)


# ------------------------------------------------------------
# 1. Load existing pilot download log
# ------------------------------------------------------------

downloads <- read_csv(
  "output/logs/sec_document_download_log.csv",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
) %>%
  filter(
    !is.na(local_file),
    file.exists(local_file)
  )

cat("Pilot documents:", nrow(downloads), "\n")

# ------------------------------------------------------------
# 2. Define flexible financial row patterns
# ------------------------------------------------------------

asset_pattern <- "total\\s+assets"

liability_pattern <- "total\\s+liabilities"

equity_pattern <- paste0(
  "(stockholders?|shareholders?|",
  "stock owners?|share owners?)",
  "['’]?s?\\s+equity|total\\s+equity"
)

heading_pattern <- paste0(
  "balance\\s+sheets?|",
  "statements?\\s+of\\s+financial\\s+condition|",
  "statements?\\s+of\\s+financial\\s+position"
)

units_pattern <- paste0(
  "in\\s+millions|in\\s+thousands|",
  "millions\\s+of\\s+dollars|",
  "thousands\\s+of\\s+dollars"
)

financial_pattern <- paste(
  asset_pattern,
  liability_pattern,
  equity_pattern,
  sep = "|"
)

# ------------------------------------------------------------
# 3. Clean text without discarding numerical cells
# ------------------------------------------------------------

clean_text <- function(x) {
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  x <- gsub(
    "\u00a0",
    " ",
    x,
    fixed = TRUE
  )
  
  str_squish(x)
}

# ------------------------------------------------------------
# 4. Inspect HTML tables
# ------------------------------------------------------------

inspect_html <- function(path) {
  
  document <- tryCatch(
    read_html(path),
    error = function(e) NULL
  )
  
  if (is.null(document)) {
    
    cat("HTML parsing unsuccessful.\n")
    
    return(invisible(NULL))
  }
  
  tables <- html_elements(
    document,
    "table"
  )
  
  cat("HTML tables:", length(tables), "\n")
  
  for (i in seq_along(tables)) {
    
    node <- tables[[i]]
    
    table_text <- clean_text(
      html_text2(node)
    )
    
    has_assets <- grepl(
      asset_pattern,
      table_text,
      ignore.case = TRUE
    )
    
    has_equity <- grepl(
      equity_pattern,
      table_text,
      ignore.case = TRUE
    )
    
    if (!has_assets || !has_equity) {
      next
    }
    
    tab <- tryCatch(
      suppressWarnings(html_table(node)),
      error = function(e) NULL
    )
    
    if (
      is.null(tab) ||
      nrow(tab) == 0
    ) {
      next
    }
    
    mat <- as.matrix(tab)
    
    row_text <- apply(
      mat,
      1,
      function(x) {
        paste(clean_text(x), collapse = " ")
      }
    )
    
    relevant_rows <- which(
      grepl(
        financial_pattern,
        row_text,
        ignore.case = TRUE
      )
    )
    
    cat("\n-----------------------------------\n")
    cat("TABLE:", i, "\n")
    cat("Rows:", nrow(mat), "\n")
    cat("Columns:", ncol(mat), "\n")
    
    cat(
      "Nested tables:",
      length(html_elements(node, "table")),
      "\n"
    )
    
    # Inspect nearby statement headings and units.
    
    preceding <- xml2::xml_find_all(
      node,
      "preceding-sibling::*[position() <= 8]"
    )
    
    preceding_text <- paste(
      clean_text(html_text2(preceding)),
      collapse = " "
    )
    
    cat(
      "Preceding context:",
      substr(preceding_text, 1, 1500),
      "\n"
    )
    
    # Preserve the original positions of header cells.
    
    cat("\nHEADER ROWS:\n")
    
    for (j in seq_len(min(6, nrow(mat)))) {
      
      cells <- clean_text(mat[j, ])
      
      cat(
        "Row", j, ":",
        paste(
          paste0(
            "[", seq_along(cells), "] ",
            cells
          ),
          collapse = " | "
        ),
        "\n"
      )
    }
    
    # Preserve the original positions of financial cells.
    
    cat("\nFINANCIAL ROWS:\n")
    
    for (j in relevant_rows) {
      
      cells <- clean_text(mat[j, ])
      
      cat(
        "Row", j, ":",
        paste(
          paste0(
            "[", seq_along(cells), "] ",
            cells
          ),
          collapse = " | "
        ),
        "\n"
      )
    }
  }
  
  invisible(NULL)
}

# ------------------------------------------------------------
# 5. Inspect historical plain-text financial statements
# ------------------------------------------------------------

inspect_text <- function(path) {
  
  lines <- readLines(
    path,
    warn = FALSE,
    skipNul = TRUE
  )
  
  lines <- iconv(
    lines,
    from = "",
    to = "UTF-8",
    sub = " "
  )
  
  lines <- gsub(
    "<[^>]+>",
    " ",
    lines
  )
  
  normalized <- clean_text(lines)
  
  asset_rows <- which(
    grepl(
      asset_pattern,
      normalized,
      ignore.case = TRUE
    )
  )
  
  equity_rows <- which(
    grepl(
      equity_pattern,
      normalized,
      ignore.case = TRUE
    )
  )
  
  cat("\nPLAIN-TEXT INSPECTION\n")
  
  cat(
    "Asset row occurrences:",
    length(asset_rows),
    "\n"
  )
  
  cat(
    "Equity row occurrences:",
    length(equity_rows),
    "\n"
  )
  
  # Inspect financial statement blocks rather than
  # isolated rows appearing anywhere in the filing.
  
  for (a in asset_rows) {
    
    nearby_equity <- equity_rows[
      equity_rows > a &
        equity_rows <= a + 100
    ]
    
    if (length(nearby_equity) == 0) {
      next
    }
    
    start <- max(1, a - 35)
    
    end <- min(
      length(lines),
      nearby_equity[1] + 8
    )
    
    block <- lines[start:end]
    
    cat("\n-----------------------------------\n")
    
    cat(
      "TEXT BLOCK:",
      start, "-", end, "\n"
    )
    
    cat(
      paste(
        paste0(
          seq.int(start, end),
          ": ",
          block
        ),
        collapse = "\n"
      ),
      "\n"
    )
  }
  
  invisible(NULL)
}

# ------------------------------------------------------------
# 6. Generate calibration report
# ------------------------------------------------------------

dir.create(
  "output/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

report_path <- paste0(
  "output/logs/",
  "pilot_balance_sheet_calibration.txt"
)

report <- capture.output({
  
  for (i in seq_len(nrow(downloads))) {
    
    row <- downloads[i, ]
    
    cat("\n====================================\n")
    cat("BANK:", row$bank, "\n")
    cat("FORM:", row$form, "\n")
    cat("REPORT DATE:", row$report_date, "\n")
    cat("FILE:", row$local_file, "\n")
    cat("====================================\n")
    
    tryCatch(
      
      inspect_html(row$local_file),
      
      error = function(e) {
        cat(
          "HTML inspection error:",
          conditionMessage(e),
          "\n"
        )
      }
    )
    
    tryCatch(
      
      inspect_text(row$local_file),
      
      error = function(e) {
        cat(
          "Text inspection error:",
          conditionMessage(e),
          "\n"
        )
      }
    )
  }
})

writeLines(
  report,
  report_path,
  useBytes = TRUE
)

cat("\n====================================\n")
cat("PILOT CALIBRATION COMPLETE\n")
cat("====================================\n")

cat(
  "Documents inspected:",
  nrow(downloads),
  "\n"
)

cat(
  "Report lines:",
  length(report),
  "\n"
)

cat(
  "Report saved to:",
  report_path,
  "\n"
)
