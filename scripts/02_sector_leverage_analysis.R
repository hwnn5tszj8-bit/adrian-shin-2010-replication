library(dplyr)
library(ggplot2)
library(readr)
source("scripts/regression.R")

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Load cleaned Z.1 sector data created in Script 01
z1 <- read_csv("data/clean/z1_sector_leverage.csv", show_col_types = FALSE) %>%
  arrange(date) %>%
  mutate(quarter = 4L * lubridate::year(date) + lubridate::quarter(date))

# ------------------------------------------------------------------------------
# 1. Replicate Figures 2.1 - 2.4 (Original Sample: 1963-2006)
# ------------------------------------------------------------------------------
z1_orig <- z1 %>%
  filter(date >= as.Date("1963-01-01") & date <= as.Date("2006-12-31"))

# Reshape into long format for clean 4-panel comparison
sector_long_orig <- bind_rows(
  z1_orig %>% transmute(date, sector = "1. Households (Fig 2.1)", d_assets = d_hh_assets, d_lev = d_hh_lev),
  z1_orig %>% transmute(date, sector = "2. Nonfinancial Corporates (Fig 2.2)", d_assets = d_nf_assets, d_lev = d_nf_lev),
  z1_orig %>% transmute(date, sector = "3. Commercial Banks (Fig 2.3)", d_assets = d_cb_assets, d_lev = d_cb_lev),
  z1_orig %>% transmute(date, sector = "4. Security Brokers & Dealers (Fig 2.4)", d_assets = d_bd_assets, d_lev = d_bd_lev)
) %>%
  filter(!is.na(d_assets), !is.na(d_lev))

fig2_combined <- ggplot(sector_long_orig, aes(x = d_lev, y = d_assets)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(alpha = 0.65, color = "#1f4e78", size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#c00000", linewidth = 0.9) +
  facet_wrap(~ sector, scales = "free", ncol = 2) +
  labs(
    title = "Replication of Figures 2.1–2.4: Asset Growth vs. Leverage Growth (1963–2006)",
    subtitle = "Quarterly percentage log changes; Source: Federal Reserve Z.1 Flow of Funds",
    x = "Leverage Growth (% Quarterly)",
    y = "Total Asset Growth (% Quarterly)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11)
  )

ggsave("output/figures/fig2_1_to_2_4_replication.png", fig2_combined, width = 10, height = 7.5, dpi = 300)

# ------------------------------------------------------------------------------
# 2. Extension: Broker-Dealer Procyclicality (Pre-GFC vs. Post-GFC)
# ------------------------------------------------------------------------------
bd_compare <- z1 %>%
  filter(sample_period %in% c("Original (1963-2006)", "Post-GFC (2009-Present)")) %>%
  filter(!is.na(d_bd_assets), !is.na(d_bd_lev))

fig2_4_extension <- ggplot(bd_compare, aes(x = d_bd_lev, y = d_bd_assets)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(aes(color = sample_period), alpha = 0.7, size = 2.2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9) +
  facet_wrap(~ sample_period, ncol = 2) +
  scale_color_manual(values = c("Original (1963-2006)" = "#1f4e78", "Post-GFC (2009-Present)" = "#d95f02")) +
  labs(
    title = "Structural Shift in Broker-Dealer Leverage Procyclicality (Fig. 2.4 Extended)",
    subtitle = "Quarterly percentage log changes; Source: Federal Reserve Financial Accounts",
    x = "Leverage Growth (% Quarterly)",
    y = "Total Asset Growth (% Quarterly)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "none"
  )

ggsave("output/figures/fig2_4_pre_vs_post_gfc.png", fig2_4_extension, width = 10, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 3. Replicate & Extend Figure 3.1: Broker-Dealer Assets as % of Holding Company Assets
# ------------------------------------------------------------------------------
fig3_1_data <- z1 %>%
  filter(date >= as.Date("1980-01-01")) %>%
  mutate(bd_to_bhc_pct = 100 * (BOGZ1FL664090005Q / BOGZ1FL734090005Q)) %>%
  filter(is.finite(bd_to_bhc_pct))

fig3_1_plot <- ggplot(fig3_1_data, aes(x = date, y = bd_to_bhc_pct)) +
  annotate("rect", xmin = as.Date("2007-07-01"), xmax = as.Date("2009-06-30"),
           ymin = -Inf, ymax = Inf, fill = "gray85", alpha = 0.4) +
  geom_line(color = "#b22222", linewidth = 1.1) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray40") +
  labs(
    title = "Broker-Dealer Assets Relative to Holding Company Assets",
    subtitle = "Shaded area marks the 2007–2009 Global Financial Crisis; Source: Federal Reserve Z.1",
    x = "Date",
    y = "Security Broker-Dealer Assets (% of Holding Company Assets)"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/figures/fig3_1_bd_share_extended.png", fig3_1_plot, width = 10, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# 4. Sector regressions and the dealer slope comparison
# ------------------------------------------------------------------------------
pre <- filter(z1, sample_period == "Original (1963-2006)")
post <- filter(z1, sample_period == "Post-GFC (2009-Present)")
periods <- mutate(bd_compare, post = as.integer(sample_period == "Post-GFC (2009-Present)"))

reg_results <- bind_rows(
  fit_hac(pre, d_hh_lev ~ d_hh_assets, "Households (1963–2006)", "quarter"),
  fit_hac(pre, d_nf_lev ~ d_nf_assets, "Nonfinancial firms (1963–2006)", "quarter"),
  fit_hac(pre, d_cb_lev ~ d_cb_assets, "Commercial banks (1963–2006)", "quarter"),
  fit_hac(pre, d_bd_lev ~ d_bd_assets, "Broker-dealers (1963–2006)", "quarter"),
  fit_hac(post, d_bd_lev ~ d_bd_assets, "Broker-dealers (2009–latest)", "quarter"),
  fit_hac(periods, d_bd_lev ~ d_bd_assets * post, "Broker-dealer slope comparison", "quarter")
)
write_csv(reg_results, "output/tables/sector_leverage_regressions.csv")

# Table 3-style dynamic model for the aggregate dealer sector. The October 2006
# and January 2009 split uses the same sample flags as the scatterplots.
dynamic <- z1 %>%
  mutate(
    lag_log_leverage = if_else(quarter - lag(quarter) == 1L & lag(bd_lev) > 0,
                              log(if_else(lag(bd_lev) > 0, lag(bd_lev), NA_real_)),
                              NA_real_),
    leverage_growth = d_bd_lev / 100,
    asset_growth = d_bd_assets / 100,
    post = as.integer(sample_period == "Post-GFC (2009-Present)")
  ) %>%
  filter(sample_period %in% unique(bd_compare$sample_period))
dynamic_results <- bind_rows(
  fit_hac(filter(dynamic, post == 0L), leverage_growth ~ lag_log_leverage + asset_growth,
          "Dealer sector dynamic model (1963–2006)", "quarter"),
  fit_hac(filter(dynamic, post == 1L), leverage_growth ~ lag_log_leverage + asset_growth,
          "Dealer sector dynamic model (2009–latest)", "quarter"),
  fit_hac(dynamic, leverage_growth ~ (lag_log_leverage + asset_growth) * post,
          "Dealer sector dynamic slope comparison", "quarter")
)
write_csv(dynamic_results, "output/tables/sector_dynamic_leverage_regressions.csv")
