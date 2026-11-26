# Reconstruction of Table 3 (i)–(ii) from the matched filing subset.
library(dplyr)
library(ggplot2)
library(readr)
source("scripts/regression.R")

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

raw <- read_csv(
  "data/processed/five_bank_balance_sheets.csv",
  col_types = cols(
    .default = col_skip(), bank = col_character(), report_date = col_date(),
    accession_number = col_character(), total_assets_mn = col_double(), book_equity_mn = col_double()
  )
)
stopifnot(all(raw$total_assets_mn > raw$book_equity_mn & raw$book_equity_mn > 0))

# Map report dates to calendar quarters. Drop ambiguous duplicate quarters and
# form growth only across adjoining quarters with roughly quarterly spacing.
panel <- raw %>%
  rename(date = report_date) %>%
  mutate(quarter = 4L * lubridate::year(date) + lubridate::quarter(date)) %>%
  add_count(bank, quarter, name = "reports_in_quarter") %>%
  filter(reports_in_quarter == 1L) %>%
  arrange(bank, date) %>%
  group_by(bank) %>%
  mutate(
    log_assets = log(total_assets_mn),
    log_leverage = log(total_assets_mn / book_equity_mn),
    adjacent = quarter - lag(quarter) == 1L & between(as.integer(date - lag(date)), 70L, 115L),
    lag_leverage = if_else(adjacent, lag(log_leverage), NA_real_),
    asset_growth = if_else(adjacent, log_assets - lag(log_assets), NA_real_),
    leverage_growth = if_else(adjacent, log_leverage - lag(log_leverage), NA_real_)
  ) %>%
  ungroup()

sample <- filter(panel, adjacent)
coverage <- raw %>%
  count(bank, name = "Matched_Balance_Sheets") %>%
  left_join(count(sample, bank, name = "Regression_Quarters"), by = "bank") %>%
  mutate(Regression_Quarters = coalesce(Regression_Quarters, 0L))
write_csv(coverage, "output/tables/five_bank_coverage.csv")
if (n_distinct(sample$bank) != 5L) stop("Table 3 requires consecutive matched quarters for all five banks.")

table3 <- bind_rows(
  fit_hac(sample, leverage_growth ~ lag_leverage + factor(bank),
          "Table 3 (i) / matched filing subset", "quarter"),
  fit_hac(sample, leverage_growth ~ lag_leverage + asset_growth + factor(bank),
          "Table 3 (ii) / matched filing subset", "quarter")
)
write_csv(table3, "output/tables/table3_five_bank_leverage_regressions.csv")

chart <- ggplot(sample, aes(100 * asset_growth, 100 * leverage_growth)) +
  geom_hline(yintercept = 0, colour = "grey78") +
  geom_vline(xintercept = 0, colour = "grey78") +
  geom_point(colour = "#225477", alpha = 0.75) +
  facet_wrap(~ bank) +
  labs(
    title = "Asset growth and leverage growth at the five investment banks",
    subtitle = "Matched consecutive filing quarters",
    x = "Asset growth (quarterly log percent)", y = "Leverage growth (quarterly log percent)"
  ) +
  theme_minimal(base_size = 12)
ggsave("output/figures/five_bank_leverage_growth.png", chart, width = 9, height = 5.5, dpi = 240)

# Compare against the nine original pilot transcriptions for audit purposes.
pilot <- read_csv(
  "data/processed/pilot_balance_sheets_reconciled.csv",
  col_types = cols(.default = col_skip(), bank = col_character(), report_date = col_date(),
                   total_assets = col_double(), book_equity = col_double())
)
pilot_comparison <- pilot %>%
  left_join(raw, by = c("bank", "report_date")) %>%
  mutate(status = case_when(
    is.na(accession_number) ~ "No matched statement",
    total_assets == total_assets_mn & book_equity == book_equity_mn ~ "Agrees with filing",
    TRUE ~ "Pilot and filing differ"
  ))
write_csv(pilot_comparison, "output/tables/five_bank_pilot_comparison.csv")
