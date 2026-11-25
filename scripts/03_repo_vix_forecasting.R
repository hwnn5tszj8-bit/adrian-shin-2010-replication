# ==============================================================================
# Script 03: Weekly Primary Dealer Repos & VIX / Volatility Risk Premium
# Replicates & Extends Table 4, Table 5, and Figure 4.1 (Adrian & Shin, 2010)
# ==============================================================================

packages <- c("tidyverse", "lubridate", "sandwich", "lmtest")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)

# ------------------------------------------------------------------------------
# 1. Load & Splice Weekly NY Fed Primary Dealer Repo Data (1998 - Present)
# ------------------------------------------------------------------------------
nyfed_raw <- read_csv("data/raw/nyfed_all_timeseries.csv", show_col_types = FALSE) %>%
  mutate(
    date  = as.Date(`As Of Date`),
    code  = str_trim(`Time Series`),
    value = suppressWarnings(as.numeric(`Value (millions)`))
  ) %>%
  filter(!is.na(date), !is.na(value))

# Era 1 (1998 - Jun 2001): Total Repos & Reverse Repos
era1 <- nyfed_raw %>%
  filter(code %in% c("PDFRPTA", "PDFRRPTA")) %>%
  pivot_wider(id_cols = date, names_from = code, values_from = value, values_fn = mean) %>%
  transmute(date, repo = PDFRPTA, rev_repo = PDFRRPTA)

# Era 2 (Jul 2001 - Mar 2013): Total Repos & Reverse Repos
era2 <- nyfed_raw %>%
  filter(code %in% c("PDFMRPTA", "PDFMRRPTA")) %>%
  pivot_wider(id_cols = date, names_from = code, values_from = value, values_fn = mean) %>%
  transmute(date, repo = PDFMRPTA, rev_repo = PDFMRRPTA)

# Era 3 (Apr 2013 - Present): Sum across all 9 collateral Total buckets
repo_codes_2013 <- c(
  "PDSORA-UTSETTOT", "PDSORA-UTSTTOT", "PDSORA-FGEMTOT",
  "PDSORA-FGMTOT",   "PDSORA-FGCMTOT", "PDSORA-CDTOT",
  "PDSORA-ABSTOT",   "PDSORA-ETOT",    "PDSORA-OTOT"
)

rev_repo_codes_2013 <- c(
  "PDSIRRA-UTSETTOT", "PDSIRRA-UTSTTOT", "PDSIRRA-FGEMTOT",
  "PDSIRRA-FGMTOT",   "PDSIRRA-FGCMTOT", "PDSIRRA-CDTOT",
  "PDSIRRA-ABSTOT",   "PDSIRRA-ETOT",    "PDSIRRA-OTOT"
)

era3_repo <- nyfed_raw %>%
  filter(code %in% repo_codes_2013) %>%
  group_by(date) %>%
  summarise(repo = sum(value, na.rm = TRUE), .groups = "drop")

era3_rev <- nyfed_raw %>%
  filter(code %in% rev_repo_codes_2013) %>%
  group_by(date) %>%
  summarise(rev_repo = sum(value, na.rm = TRUE), .groups = "drop")

era3 <- full_join(era3_repo, era3_rev, by = "date")

# Combine all eras cleanly and compute weekly log growth rates (%)
dealer_weekly <- bind_rows(era1, era2, era3) %>%
  arrange(date) %>%
  distinct(date, .keep_all = TRUE) %>%
  filter(repo > 0, rev_repo > 0) %>%
  mutate(
    # Scale to $ Billions for cleaner summary stats
    repo_bn     = repo / 1000,
    rev_repo_bn = rev_repo / 1000,
    net_repo_bn = (repo - rev_repo) / 1000,
    # Weekly percentage log growth rates
    d_repo      = 100 * (log(repo) - lag(log(repo))),
    d_rev_repo  = 100 * (log(rev_repo) - lag(log(rev_repo))),
    d_net_repo  = d_repo - d_rev_repo
  )

# ------------------------------------------------------------------------------
# 2. Align Daily VIX & S&P 500 with Weekly Wednesday Reporting Dates
# ------------------------------------------------------------------------------
mkt_daily <- read_csv("data/raw/fred_vix_sp500_daily.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(
    sp500_ret = 100 * (log(SP500) - lag(log(SP500)))
  )

# Assign each trading day to the Primary Dealer reporting week ending Wednesday
# Weekly realized volatility = annualized std dev of daily S&P 500 returns
# VIX_t = Wednesday closing VIX; VRP_t = VIX_t - Realized_Vol_t
weekly_mkt <- dealer_weekly %>%
  select(week_end = date) %>%
  mutate(week_start = week_end - days(6)) %>%
  rowwise() %>%
  mutate(
    vix_wed = {
      sub <- mkt_daily %>% filter(date <= week_end, date >= week_end - days(3), !is.na(VIXCLS))
      if (nrow(sub) > 0) tail(sub$VIXCLS, 1) else NA_real_
    },
    sp500_wk_ret = {
      sub <- mkt_daily %>% filter(date >= week_start, date <= week_end, !is.na(sp500_ret))
      if (nrow(sub) > 0) sum(sub$sp500_ret, na.rm = TRUE) else NA_real_
    },
    realized_vol = {
      sub <- mkt_daily %>% filter(date >= week_start, date <= week_end, !is.na(sp500_ret))
      if (nrow(sub) >= 3) sd(sub$sp500_ret, na.rm = TRUE) * sqrt(252) else NA_real_
    }
  ) %>%
  ungroup() %>%
  mutate(
    vrp = vix_wed - realized_vol
  )

# Merge dealer financing with weekly market volatility & create lead/lag terms
panel_weekly <- dealer_weekly %>%
  left_join(weekly_mkt, by = c("date" = "week_end")) %>%
  arrange(date) %>%
  mutate(
    # Forward (t+1) outcomes for predictive regressions (Table 5)
    vix_lead1    = lead(vix_wed, 1),
    d_vix_lead1  = lead(vix_wed, 1) - vix_wed,
    vrp_lead1    = lead(vrp, 1),
    # Regime tags
    regime = case_when(
      date <= as.Date("2007-06-30") ~ "Pre-GFC (1998-2007)",
      date >= as.Date("2009-07-01") ~ "Post-GFC (2009-Present)",
      TRUE ~ "GFC (2007-2009)"
    )
  )

write_csv(panel_weekly, "data/clean/weekly_dealer_vix_panel.csv")

# ------------------------------------------------------------------------------
# 3. Replicate & Extend Table 4: Weekly Summary Statistics
# ------------------------------------------------------------------------------
calc_summary <- function(df, regime_label) {
  vars <- c("repo_bn", "rev_repo_bn", "d_repo", "d_rev_repo", "vix_wed", "realized_vol", "vrp")
  map_dfr(vars, function(v) {
    x <- df[[v]]
    x <- x[!is.na(x)]
    tibble(
      Regime   = regime_label,
      Variable = v,
      Mean     = round(mean(x), 2),
      Std_Dev  = round(sd(x), 2),
      Median   = round(median(x), 2),
      Min      = round(min(x), 2),
      Max      = round(max(x), 2),
      N        = length(x)
    )
  })
}

table4 <- bind_rows(
  calc_summary(panel_weekly %>% filter(regime == "Pre-GFC (1998-2007)"), "Pre-GFC (1998-2007)"),
  calc_summary(panel_weekly %>% filter(regime == "Post-GFC (2009-Present)"), "Post-GFC (2009-Present)")
)

write_csv(table4, "output/tables/table4_weekly_summary_stats.csv")
print(table4, n = 20)

# ------------------------------------------------------------------------------
# 4. Replicate & Extend Table 5: Repo Growth Forecasting VIX & VRP (HAC SEs)
# ------------------------------------------------------------------------------
run_forecast_reg <- function(df, formula_str, model_label) {
  mod <- lm(as.formula(formula_str), data = df)
  hac <- coeftest(mod, vcov = NeweyWest(mod, lag = 4, prewhite = FALSE))
  tibble(
    Model         = model_label,
    Term          = rownames(hac),
    Estimate      = round(hac[, 1], 4),
    Std_Error_HAC = round(hac[, 2], 4),
    t_stat        = round(hac[, 3], 2),
    p_value       = round(hac[, 4], 4),
    R_squared     = round(summary(mod)$r.squared, 3),
    N             = nobs(mod)
  )
}

pre_df     <- panel_weekly %>% filter(regime == "Pre-GFC (1998-2007)")
post_df    <- panel_weekly %>% filter(regime == "Post-GFC (2009-Present)")
modern_vrp <- panel_weekly %>% filter(!is.na(vrp), !is.na(vrp_lead1), !is.na(sp500_wk_ret))

table5 <- bind_rows(
  # Pre-GFC Full Sample (1998 - 2007): VIX Forecasting
  run_forecast_reg(pre_df,     "vix_lead1 ~ d_repo + vix_wed",                "1. Pre-GFC (1998-2007): VIX(t+1)"),
  run_forecast_reg(pre_df,     "d_vix_lead1 ~ d_repo + vix_wed",              "2. Pre-GFC (1998-2007): dVIX(t+1)"),
  # Post-GFC Full Sample (2009 - Present): VIX Forecasting
  run_forecast_reg(post_df,    "vix_lead1 ~ d_repo + vix_wed",                "3. Post-GFC (2009-Present): VIX(t+1)"),
  run_forecast_reg(post_df,    "d_vix_lead1 ~ d_repo + vix_wed",              "4. Post-GFC (2009-Present): dVIX(t+1)"),
  # Modern S&P 500 Window (2016 - Present): VIX + Equity Return + VRP
  run_forecast_reg(modern_vrp, "vix_lead1 ~ d_repo + vix_wed + sp500_wk_ret", "5. Modern (2016+): VIX(t+1) + S&P500"),
  run_forecast_reg(modern_vrp, "vrp_lead1 ~ d_repo + vrp + sp500_wk_ret",     "6. Modern (2016+): VRP(t+1) + S&P500")
)

write_csv(table5, "output/tables/table5_vix_forecasting_regressions.csv")
print(table5 %>% filter(Term == "d_repo"))

# ------------------------------------------------------------------------------
# 5. Replicate & Extend Figure 4.1: VIX vs. Lagged Repo Growth
# ------------------------------------------------------------------------------
fig4_data <- panel_weekly %>%
  filter(regime %in% c("Pre-GFC (1998-2007)", "Post-GFC (2009-Present)")) %>%
  filter(!is.na(d_repo), !is.na(vix_lead1)) %>%
  mutate(regime = factor(regime, levels = c("Pre-GFC (1998-2007)", "Post-GFC (2009-Present)")))

fig4_1 <- ggplot(fig4_data, aes(x = d_repo, y = vix_lead1)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(aes(color = regime), alpha = 0.45, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9) +
  facet_wrap(~ regime, ncol = 2) +
  scale_color_manual(values = c("Pre-GFC (1998-2007)" = "#1f4e78", "Post-GFC (2009-Present)" = "#d95f02")) +
  labs(
    title = "Replication & Extension of Figure 4.1: Next-Week VIX vs. Weekly Dealer Repo Growth",
    subtitle = "Weekly Wednesday Primary Dealer Repo growth (%) vs. VIX(t+1); Source: NY Fed FR 2004 & CBOE",
    x = "Weekly Primary Dealer Repo Growth (%)",
    y = "Next-Week CBOE VIX Level"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

ggsave("output/figures/fig4_1_vix_vs_repo_growth.png", fig4_1, width = 10, height = 5, dpi = 300)

cat("\nSuccess! Saved Table 4, Table 5, and Figure 4.1 to output/.\n")
panel_weekly <- panel_weekly %>%
  mutate(
    across(c(d_repo, d_rev_repo, d_net_repo), ~ ifelse(date %in% as.Date(c("2001-07-04", "2013-04-03")), NA_real_, .x))
  )

write_csv(panel_weekly, "data/clean/weekly_dealer_vix_panel.csv")

# Re-run Table 4 & Table 5 with clean splice boundaries
table4 <- bind_rows(
  calc_summary(panel_weekly %>% filter(regime == "Pre-GFC (1998-2007)"), "Pre-GFC (1998-2007)"),
  calc_summary(panel_weekly %>% filter(regime == "Post-GFC (2009-Present)"), "Post-GFC (2009-Present)")
)
write_csv(table4, "output/tables/table4_weekly_summary_stats.csv")
print(table4 %>% filter(Variable %in% c("repo_bn", "rev_repo_bn", "d_repo", "d_rev_repo", "vix_wed")))

pre_df  <- panel_weekly %>% filter(regime == "Pre-GFC (1998-2007)")
post_df <- panel_weekly %>% filter(regime == "Post-GFC (2009-Present)")

table5 <- bind_rows(
  run_forecast_reg(pre_df,  "vix_lead1 ~ d_repo + vix_wed",   "1. Pre-GFC (1998-2007): VIX(t+1)"),
  run_forecast_reg(pre_df,  "d_vix_lead1 ~ d_repo + vix_wed", "2. Pre-GFC (1998-2007): dVIX(t+1)"),
  run_forecast_reg(post_df, "vix_lead1 ~ d_repo + vix_wed",   "3. Post-GFC (2009-Present): VIX(t+1)"),
  run_forecast_reg(post_df, "d_vix_lead1 ~ d_repo + vix_wed", "4. Post-GFC (2009-Present): dVIX(t+1)")
)
write_csv(table5, "output/tables/table5_vix_forecasting_regressions.csv")
print(table5 %>% filter(Term == "d_repo"))