# New York Fed Primary Dealer financing and CBOE VIX, weekly.
library(dplyr)
library(ggplot2)
library(readr)
source("scripts/regression.R")

dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

financing <- read_csv(
  "data/raw/nyfed_all_timeseries.csv", col_types = cols(.default = col_character())
) %>%
  transmute(date = as.Date(`As Of Date`), series = `Time Series`, raw_value = `Value (millions)`)

# Keep the published historical totals. The 2013 reporting redesign replaces
# them with collateral categories. Use the well-covered agency MBS repo series
# as a separately reported modern proxy, since other buckets are suppressed.
repo_series <- tibble::tribble(
  ~series, ~scope, ~start, ~end,
  "PDFRPTA", "Historical overlap", "1998-01-28", "2001-06-27",
  "PDFMRPTA", "Historical overlap", "2001-07-04", "2008-04-02",
  "PDFMRPTA", "Post-crisis legacy total", "2009-07-01", "2013-03-27",
  "PDSORA-FGMTOT", "Modern agency MBS proxy", "2013-04-03", "2099-01-01"
)
repo <- inner_join(financing, repo_series, by = "series", relationship = "many-to-many") %>%
  filter(date >= as.Date(start), date <= as.Date(end)) %>%
  mutate(repo_mn = suppressWarnings(as.numeric(raw_value))) %>%
  select(date, series, scope, repo_mn) %>%
  arrange(date) %>%
  mutate(log_repo = log(if_else(repo_mn > 0, repo_mn, NA_real_)))
stopifnot(!anyDuplicated(repo$date), all(lubridate::wday(repo$date) == 4L))

# Window starting Wednesday t ends Tuesday t+6. Its VIX change is relative
# to the preceding window, whose mean is observable by Wednesday t.
market <- read_csv(
  "data/raw/fred_vix_sp500_daily.csv",
  col_select = c(date, VIXCLS), show_col_types = FALSE
)
vix <- market %>%
  filter(is.finite(VIXCLS)) %>%
  mutate(date = lubridate::floor_date(date, "week", week_start = 3)) %>%
  group_by(date) %>%
  summarise(vix_average = mean(VIXCLS), vix_days = n(), .groups = "drop")

panel <- repo %>%
  mutate(
    week = as.integer(date - as.Date("1990-01-03")) %/% 7L,
    repo_growth = if_else(
      as.integer(date - lag(date)) == 7L & series == lag(series) & scope == lag(scope),
      100 * (log_repo - lag(log_repo)),
      NA_real_
    )
  ) %>%
  left_join(vix, by = "date") %>%
  left_join(
    transmute(vix, date = date + 7L, lag_vix = vix_average, lag_vix_days = vix_days),
    by = "date"
  ) %>%
  mutate(
    vix_change = if_else(
      date + 6L <= max(market$date) & vix_days >= 3L & lag_vix_days >= 3L,
      vix_average - lag_vix, NA_real_
    )
  )
write_csv(panel, "data/clean/weekly_dealer_vix_panel.csv")

summary <- panel %>%
  group_by(scope) %>%
  summarise(
    First = min(date), Last = max(date), Weeks = n(),
    Repo_Reported = sum(is.finite(repo_mn)),
    Repo_Growth_Reported = sum(is.finite(repo_growth)),
    Repo_Mean_Billions = mean(repo_mn, na.rm = TRUE) / 1000,
    Repo_Growth_Mean = mean(repo_growth, na.rm = TRUE),
    Repo_Growth_SD = sd(repo_growth, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(summary, "output/tables/table4_weekly_summary_stats.csv")

estimation <- filter(panel, is.finite(repo_growth), is.finite(vix_change), is.finite(lag_vix))
groups <- split(estimation, factor(estimation$scope, levels = unique(repo_series$scope)))
table5 <- bind_rows(lapply(names(groups), function(scope) {
  sample <- groups[[scope]]
  bind_rows(
    fit_hac(sample, vix_change ~ lag_vix, paste(scope, "benchmark", sep = " / "), "week"),
    fit_hac(sample, vix_change ~ lag_vix + repo_growth, paste(scope, "with repo", sep = " / "), "week")
  )
}))
write_csv(table5, "output/tables/table5_vix_forecasting_regressions.csv")

historical <- groups[["Historical overlap"]]
model <- lm(vix_change ~ lag_vix + repo_growth, data = historical)
historical$fitted_at_mean_vix <- predict(
  model, newdata = mutate(historical, lag_vix = mean(lag_vix))
)
chart <- ggplot(historical, aes(repo_growth, vix_change)) +
  geom_hline(yintercept = 0, colour = "grey75") +
  geom_point(alpha = 0.45, colour = "#225477") +
  geom_line(aes(y = fitted_at_mean_vix), linewidth = 0.9, colour = "#AF4319") +
  labs(
    title = "Weekly changes in VIX and lagged dealer repo growth",
    subtitle = "1998–2008 overlap. Fitted line holds the prior VIX average at its sample mean.",
    x = "Primary dealer repo growth (%)", y = "Change in average VIX (points)"
  ) +
  theme_minimal(base_size = 13)
ggsave("output/figures/fig4_1_vix_vs_repo_growth.png", chart, width = 9, height = 5, dpi = 240)
