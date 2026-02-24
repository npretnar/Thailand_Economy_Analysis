###############################################################################
# 06_structural_transformation.R
# Structural transformation analysis — Thailand vs peers
#
# Analyses:
#   1. Output shares vs employment shares (Thailand)
#   2. Relative labor productivity by sector
#   3. Structural change index (Michaely index by decade)
#   4. Agriculture employment vs GDP per capita (cross-country scatter)
#   5. Productivity gap — agriculture vs non-agriculture (Lewis dual economy)
#   6. Shift-share decomposition (McMillan & Rodrik 2011)
#
# Data sources:
#   - data/wb_full_panel.csv (GDP, sectoral output shares, GDP per capita)
#   - data/wb_labor_by_sector.csv (employment shares, labor force)
###############################################################################

# --- 1. Setup ----------------------------------------------------------------

required_packages <- c("tidyverse", "scales")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(tidyverse)
library(scales)

# --- 2. Load data ------------------------------------------------------------

cat("Loading data...\n")
gdp_panel   <- read_csv("data/wb_full_panel.csv", show_col_types = FALSE)
labor_panel <- read_csv("data/wb_labor_by_sector.csv", show_col_types = FALSE)

cat("  GDP panel:", nrow(gdp_panel), "rows\n")
cat("  Labor panel:", nrow(labor_panel), "rows\n")

# --- 3. Merge ----------------------------------------------------------------

# Keep relevant columns from each source and merge
gdp_cols <- gdp_panel %>%
  select(iso3c, country, year, country_group, is_thailand,
         gdp_current_usd, gdp_constant_2015_usd,
         gdp_pc_current_usd, gdp_pc_constant_2015,
         gdp_ppp_current, gdp_pc_ppp_current, population,
         agriculture_pct_gdp, industry_pct_gdp, services_pct_gdp)

labor_cols <- labor_panel %>%
  select(iso3c, year, short_name, empl_agri, empl_ind, empl_svc, labor_force)

panel <- gdp_cols %>%
  inner_join(labor_cols, by = c("iso3c", "year")) %>%
  filter(!is.na(agriculture_pct_gdp), !is.na(empl_agri),
         !is.na(gdp_current_usd), !is.na(labor_force))

cat("  Merged panel:", nrow(panel), "rows,", n_distinct(panel$iso3c), "countries\n")

# Convenience subsets
thailand <- panel %>% filter(iso3c == "THA")
peers    <- panel %>% filter(iso3c %in% c("THA", "MYS", "IDN", "PHL", "VNM",
                                           "KOR", "CHN", "JPN"))

# --- 4. Theme & palettes ----------------------------------------------------

thai_red   <- "#E3120B"
grey_other <- "#AAAAAA"
blue_dev   <- "#2166AC"

sector_colors <- c(
  "Agriculture" = "#66C2A5",
  "Industry"    = "#FC8D62",
  "Services"    = "#8DA0CB"
)

theme_thai <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey40", size = 11),
    plot.caption  = element_text(color = "grey60", size = 9, hjust = 0),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

save_plot <- function(p, filename, w = 10, h = 7) {
  ggsave(filename, plot = p, width = w, height = h, dpi = 300, bg = "white")
  cat("  Saved:", filename, "\n")
}

# =============================================================================
# ANALYSIS 1: Output shares vs employment shares — Thailand
# =============================================================================

cat("\n--- Analysis 1: Output vs Employment Shares (Thailand) ---\n")

p35_data <- thailand %>%
  select(year,
         agriculture_pct_gdp, industry_pct_gdp, services_pct_gdp,
         empl_agri, empl_ind, empl_svc) %>%
  pivot_longer(-year, names_to = "var", values_to = "pct") %>%
  mutate(
    sector = case_when(
      grepl("agri", var) ~ "Agriculture",
      grepl("ind", var)  ~ "Industry",
      grepl("svc|serv", var) ~ "Services"
    ),
    measure = ifelse(grepl("pct_gdp", var), "GDP Share", "Employment Share")
  )

p35 <- ggplot(p35_data, aes(x = year, y = pct, color = measure, linetype = measure)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~sector, scales = "free_y") +
  scale_color_manual(values = c("GDP Share" = "#2166AC", "Employment Share" = "#B2182B"),
                     name = NULL) +
  scale_linetype_manual(values = c("GDP Share" = "solid", "Employment Share" = "dashed"),
                        name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Output vs Employment Shares by Sector — Thailand",
    subtitle = "Gaps reveal relative productivity differences across sectors",
    x = NULL, y = "Share (%)",
    caption  = "Source: World Bank WDI. GDP shares = value added; employment shares = ILO estimates."
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 12))

save_plot(p35, "plots/35_output_vs_employment_thailand.png", w = 14, h = 6)

# =============================================================================
# ANALYSIS 2: Relative labor productivity by sector
# =============================================================================

cat("\n--- Analysis 2: Relative Labor Productivity ---\n")

# Compute sector value added per worker, indexed to economy-wide average
panel <- panel %>%
  mutate(
    # Sector VA (current USD)
    va_agri = agriculture_pct_gdp / 100 * gdp_current_usd,
    va_ind  = industry_pct_gdp    / 100 * gdp_current_usd,
    va_svc  = services_pct_gdp   / 100 * gdp_current_usd,
    # Sector employment (workers)
    workers_agri = empl_agri / 100 * labor_force,
    workers_ind  = empl_ind  / 100 * labor_force,
    workers_svc  = empl_svc  / 100 * labor_force,
    # Sector VA per worker
    prod_agri = va_agri / workers_agri,
    prod_ind  = va_ind  / workers_ind,
    prod_svc  = va_svc  / workers_svc,
    # Economy-wide average VA per worker
    prod_avg  = gdp_current_usd / labor_force,
    # Relative productivity (indexed to average = 1.0)
    rel_prod_agri = prod_agri / prod_avg,
    rel_prod_ind  = prod_ind  / prod_avg,
    rel_prod_svc  = prod_svc  / prod_avg
  )

# Update convenience subsets
thailand <- panel %>% filter(iso3c == "THA")
peers    <- panel %>% filter(iso3c %in% c("THA", "MYS", "IDN", "PHL", "VNM",
                                           "KOR", "CHN", "JPN"))

# --- Plot 36: Thailand relative productivity by sector over time
p36_data <- thailand %>%
  select(year, rel_prod_agri, rel_prod_ind, rel_prod_svc) %>%
  pivot_longer(-year, names_to = "sector", values_to = "rel_prod") %>%
  mutate(sector = case_when(
    sector == "rel_prod_agri" ~ "Agriculture",
    sector == "rel_prod_ind"  ~ "Industry",
    sector == "rel_prod_svc"  ~ "Services"
  ))

p36 <- ggplot(p36_data, aes(x = year, y = rel_prod, color = sector)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = sector_colors, name = NULL) +
  annotate("text", x = min(p36_data$year) + 1, y = 1.05,
           label = "Economy-wide average", size = 3.2, color = "grey50", hjust = 0) +
  labs(
    title    = "Relative Labor Productivity by Sector — Thailand",
    subtitle = "Sector VA per worker / economy-wide VA per worker (1.0 = average)",
    x = NULL, y = "Relative productivity (avg = 1.0)",
    caption  = "Source: World Bank WDI. VA per worker = (sector % GDP × GDP) / (sector % empl × labor force)."
  ) +
  theme_thai

save_plot(p36, "plots/36_relative_productivity_thailand.png")

# --- Plot 37: Cross-country comparison of sectoral productivity (latest year)
p37_data <- panel %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup() %>%
  select(iso3c, short_name, country_group, rel_prod_agri, rel_prod_ind, rel_prod_svc) %>%
  pivot_longer(cols = starts_with("rel_prod"), names_to = "sector", values_to = "rel_prod") %>%
  mutate(
    sector = case_when(
      sector == "rel_prod_agri" ~ "Agriculture",
      sector == "rel_prod_ind"  ~ "Industry",
      sector == "rel_prod_svc"  ~ "Services"
    ),
    is_tha = iso3c == "THA"
  )

# Sort by agriculture relative productivity
agri_order <- p37_data %>%
  filter(sector == "Agriculture") %>% arrange(rel_prod) %>% pull(short_name)
p37_data <- p37_data %>%
  mutate(short_name = factor(short_name, levels = agri_order))

p37 <- ggplot(p37_data, aes(x = short_name, y = rel_prod)) +
  geom_col(aes(fill = ifelse(iso3c == "THA", "Thailand", "Other")),
           width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  scale_fill_manual(values = c("Thailand" = thai_red, "Other" = "#8DA0CB")) +
  facet_wrap(~sector, scales = "free_x") +
  coord_flip() +
  labs(
    title    = "Relative Labor Productivity by Sector — Cross-Country Comparison",
    subtitle = "Sector VA per worker relative to economy-wide average (1.0 = average); latest year",
    x = NULL, y = "Relative productivity (avg = 1.0)",
    caption  = "Source: World Bank WDI."
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 12),
        panel.grid.major.y = element_blank())

save_plot(p37, "plots/37_relative_productivity_comparison.png", w = 14, h = 7)

# Print summary
cat("\n  Thailand relative productivity (latest year):\n")
tha_latest <- thailand %>% slice_max(year, n = 1)
cat(sprintf("    Agriculture: %.2f\n", tha_latest$rel_prod_agri))
cat(sprintf("    Industry:    %.2f\n", tha_latest$rel_prod_ind))
cat(sprintf("    Services:    %.2f\n", tha_latest$rel_prod_svc))

# =============================================================================
# ANALYSIS 3: Structural Change Index (Michaely Index by Decade)
# =============================================================================

cat("\n--- Analysis 3: Structural Change Index (Michaely) ---\n")

# Assign decades
michaely_data <- panel %>%
  mutate(decade = case_when(
    year >= 1991 & year <= 1999 ~ "1990s",
    year >= 2000 & year <= 2009 ~ "2000s",
    year >= 2010 & year <= 2019 ~ "2010s",
    year >= 2020              ~ "2020s",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(decade))

# Compute Michaely index per country per decade:
# MI = (1/2) × Σ|ΔShare_i| where ΔShare is change from start to end of decade
michaely <- michaely_data %>%
  group_by(iso3c, short_name, country_group, decade) %>%
  summarise(
    start_year = min(year),
    end_year   = max(year),
    delta_agri = empl_agri[year == max(year)] - empl_agri[year == min(year)],
    delta_ind  = empl_ind[year == max(year)]  - empl_ind[year == min(year)],
    delta_svc  = empl_svc[year == max(year)]  - empl_svc[year == min(year)],
    .groups = "drop"
  ) %>%
  mutate(
    michaely_index = (abs(delta_agri) + abs(delta_ind) + abs(delta_svc)) / 2
  )

# Select key comparators for the plot
key_countries <- c("THA", "MYS", "IDN", "PHL", "VNM", "KOR", "CHN", "JPN")
p38_data <- michaely %>%
  filter(iso3c %in% key_countries) %>%
  mutate(
    is_tha = iso3c == "THA",
    short_name = fct_reorder(short_name, michaely_index, .fun = mean, .desc = TRUE)
  )

p38 <- ggplot(p38_data, aes(x = decade, y = michaely_index,
                             fill = ifelse(iso3c == "THA", "Thailand", "Other"))) +
  geom_col(width = 0.7, show.legend = FALSE) +
  scale_fill_manual(values = c("Thailand" = thai_red, "Other" = "#8DA0CB")) +
  facet_wrap(~short_name, nrow = 2) +
  labs(
    title    = "Pace of Structural Change — Michaely Index by Decade",
    subtitle = "MI = (1/2) × Σ|Δ employment share|; higher = faster sectoral reallocation",
    x = NULL, y = "Michaely index (pp)",
    caption  = "Source: World Bank WDI (ILO estimates). Employment share changes within each decade."
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 11))

save_plot(p38, "plots/38_structural_change_pace.png", w = 12, h = 7)

# Print summary
cat("\n  Michaely index by decade — Thailand:\n")
michaely %>% filter(iso3c == "THA") %>%
  select(decade, michaely_index) %>%
  mutate(michaely_index = round(michaely_index, 2)) %>%
  print()

# =============================================================================
# ANALYSIS 4: Agriculture Employment vs GDP per Capita (Cross-Country Scatter)
# =============================================================================

cat("\n--- Analysis 4: Agri Employment vs GDP/Capita Scatter ---\n")

p39_data <- panel %>%
  filter(!is.na(gdp_pc_ppp_current), !is.na(empl_agri)) %>%
  mutate(is_tha = iso3c == "THA")

# Latest year labels for key countries
p39_labels <- p39_data %>%
  filter(iso3c %in% key_countries) %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p39 <- ggplot(p39_data, aes(x = gdp_pc_ppp_current, y = empl_agri)) +
  # Background: all country-year observations
  geom_point(data = p39_data %>% filter(!is_tha),
             color = grey_other, alpha = 0.25, size = 1) +
  # LOESS fit line
  geom_smooth(method = "loess", se = TRUE, color = blue_dev,
              fill = blue_dev, alpha = 0.15, linewidth = 1) +
  # Thailand trajectory
  geom_path(data = p39_data %>% filter(is_tha),
            color = thai_red, linewidth = 1.1, alpha = 0.8) +
  geom_point(data = p39_data %>% filter(is_tha),
             color = thai_red, size = 1.5) +
  # Labels at latest year
  geom_text(data = p39_labels,
            aes(label = short_name),
            size = 3, fontface = "bold",
            nudge_y = 1.5, check_overlap = TRUE) +
  scale_x_log10(labels = label_dollar()) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "The Structural Transformation Curve",
    subtitle = "Agriculture employment share vs GDP per capita (PPP); Thailand trajectory in red",
    x = "GDP per capita, PPP (log scale, current international $)",
    y = "Agriculture employment share (%)",
    caption  = "Source: World Bank WDI. Each point = one country-year. LOESS fit across all observations."
  ) +
  theme_thai

save_plot(p39, "plots/39_agri_empl_vs_gdppc_scatter.png")

# =============================================================================
# ANALYSIS 5: Productivity Gap — Agriculture vs Non-Agriculture (Lewis Model)
# =============================================================================

cat("\n--- Analysis 5: Dual Economy Productivity Gap ---\n")

panel <- panel %>%
  mutate(
    # Non-agriculture VA and workers
    va_nonagri      = va_ind + va_svc,
    workers_nonagri = workers_ind + workers_svc,
    # VA per worker
    prod_nonagri    = va_nonagri / workers_nonagri,
    # Productivity ratio: non-agri / agri
    prod_gap_ratio  = prod_nonagri / prod_agri
  )

# Update subsets
thailand <- panel %>% filter(iso3c == "THA")
peers    <- panel %>% filter(iso3c %in% key_countries)

# --- Plot 40: Productivity gap ratio over time — Thailand vs peers
p40_data <- peers %>%
  select(iso3c, short_name, year, prod_gap_ratio) %>%
  filter(is.finite(prod_gap_ratio))

p40 <- ggplot(p40_data, aes(x = year, y = prod_gap_ratio, group = iso3c)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_line(data = p40_data %>% filter(iso3c != "THA"),
            color = grey_other, linewidth = 0.6, alpha = 0.5) +
  geom_line(data = p40_data %>% filter(iso3c == "THA"),
            color = thai_red, linewidth = 1.3) +
  # End-of-line labels
  geom_text(data = p40_data %>% group_by(iso3c) %>% slice_max(year, n = 1),
            aes(label = short_name),
            hjust = -0.1, size = 3, fontface = "bold",
            color = ifelse(
              (p40_data %>% group_by(iso3c) %>% slice_max(year, n = 1))$iso3c == "THA",
              thai_red, "grey40")) +
  annotate("text", x = min(p40_data$year) + 1, y = 1.1,
           label = "Parity (1.0)", size = 3, color = "grey50", hjust = 0) +
  scale_y_continuous(labels = label_number()) +
  coord_cartesian(xlim = c(min(p40_data$year), max(p40_data$year) + 3)) +
  labs(
    title    = "Dual Economy Productivity Gap",
    subtitle = "Non-agriculture / agriculture VA per worker; higher = larger misallocation",
    x = NULL, y = "Productivity ratio (non-agri / agri)",
    caption  = "Source: World Bank WDI. Lewis-model diagnostic: ratio > 1 implies scope for reallocation gains."
  ) +
  theme_thai

save_plot(p40, "plots/40_productivity_gap_dual_economy.png")

tha_last_gap <- thailand %>% slice_max(year, n = 1)
cat(sprintf("\n  Thailand non-agri/agri productivity ratio (latest): %.2f\n",
            tha_last_gap$prod_gap_ratio))

# =============================================================================
# ANALYSIS 6: Shift-Share Decomposition (McMillan & Rodrik 2011)
# =============================================================================

cat("\n--- Analysis 6: Shift-Share Decomposition ---\n")

# Decompose aggregate productivity growth into:
#   Within-sector effect:  Σ θ_i,t0 × Δπ_i
#   Between-sector effect: Σ π_i,T  × Δθ_i
#   Cross-term:            Σ Δπ_i   × Δθ_i
# Where θ_i = employment share of sector i, π_i = VA per worker in sector i

# We do this over the full period and by sub-period
shift_share <- function(df, start_yr, end_yr) {
  # Use nearest available year if exact year has missing sectoral GDP data
  df_complete <- df %>%
    filter(!is.na(agriculture_pct_gdp), !is.na(industry_pct_gdp),
           !is.na(services_pct_gdp), !is.na(gdp_constant_2015_usd))
  d0 <- df_complete %>% filter(year >= start_yr) %>% slice_min(year, n = 1)
  d1 <- df_complete %>% filter(year <= end_yr)   %>% slice_max(year, n = 1)

  if (nrow(d0) == 0 | nrow(d1) == 0) return(NULL)
  if (d0$year >= d1$year) return(NULL)

  sectors <- c("agri", "ind", "svc")

  # Employment shares (as fractions)
  theta_0 <- c(d0$empl_agri, d0$empl_ind, d0$empl_svc) / 100
  theta_1 <- c(d1$empl_agri, d1$empl_ind, d1$empl_svc) / 100

  # VA per worker (in constant 2015 USD for comparability)
  # Use constant GDP to get real productivity
  gdp0 <- d0$gdp_constant_2015_usd
  gdp1 <- d1$gdp_constant_2015_usd
  lf0  <- d0$labor_force
  lf1  <- d1$labor_force

  pi_0 <- c(d0$agriculture_pct_gdp, d0$industry_pct_gdp, d0$services_pct_gdp) / 100 * gdp0 /
           (theta_0 * lf0)
  pi_1 <- c(d1$agriculture_pct_gdp, d1$industry_pct_gdp, d1$services_pct_gdp) / 100 * gdp1 /
           (theta_1 * lf1)

  delta_theta <- theta_1 - theta_0
  delta_pi    <- pi_1 - pi_0

  within  <- sum(theta_0 * delta_pi)
  between <- sum(pi_1 * delta_theta)
  cross   <- sum(delta_pi * delta_theta)

  # Express as % of initial aggregate productivity
  agg_prod_0 <- gdp0 / lf0
  tibble(
    within_effect  = within  / agg_prod_0 * 100,
    between_effect = between / agg_prod_0 * 100,
    cross_effect   = cross   / agg_prod_0 * 100,
    total_growth   = (within + between + cross) / agg_prod_0 * 100
  )
}

# Apply to key countries over full period and sub-periods
periods <- list(
  "Full (1991-2024)" = c(1991, 2024),
  "1991-2000"        = c(1991, 2000),
  "2000-2010"        = c(2000, 2010),
  "2010-2024"        = c(2010, 2024)
)

ss_results <- bind_rows(lapply(key_countries, function(cc) {
  cdf <- panel %>% filter(iso3c == cc)
  bind_rows(lapply(names(periods), function(pname) {
    yrs <- periods[[pname]]
    res <- shift_share(cdf, yrs[1], yrs[2])
    if (!is.null(res)) {
      res %>% mutate(iso3c = cc,
                     short_name = cdf$short_name[1],
                     period = pname)
    }
  }))
}))

cat("  Shift-share results:", nrow(ss_results), "country-period observations\n")

# --- Plot 41: Shift-share decomposition — Thailand by sub-period
p41_data <- ss_results %>%
  filter(iso3c == "THA", period != "Full (1991-2024)") %>%
  select(period, within_effect, between_effect, cross_effect) %>%
  pivot_longer(-period, names_to = "component", values_to = "pct") %>%
  mutate(
    component = case_when(
      component == "within_effect"  ~ "Within-sector",
      component == "between_effect" ~ "Between-sector\n(structural change)",
      component == "cross_effect"   ~ "Cross-term"
    ),
    component = factor(component, levels = c("Within-sector",
                                              "Between-sector\n(structural change)",
                                              "Cross-term"))
  )

p41 <- ggplot(p41_data, aes(x = period, y = pct, fill = component)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_fill_manual(values = c("Within-sector" = "#66C2A5",
                                "Between-sector\n(structural change)" = "#FC8D62",
                                "Cross-term" = "#8DA0CB"),
                    name = NULL) +
  labs(
    title    = "Shift-Share Decomposition of Productivity Growth — Thailand",
    subtitle = "% of initial period aggregate labor productivity; McMillan & Rodrik (2011) method",
    x = NULL, y = "Contribution to productivity growth (%)",
    caption  = "Source: World Bank WDI. Constant 2015 USD. Within = productivity growth holding shares constant;\nBetween = reallocation effect; Cross = interaction."
  ) +
  theme_thai

save_plot(p41, "plots/41_shift_share_decomposition_thailand.png")

# --- Plot 42: Shift-share comparison across countries (full period)
p42_data <- ss_results %>%
  filter(period == "Full (1991-2024)") %>%
  select(iso3c, short_name, within_effect, between_effect, cross_effect) %>%
  pivot_longer(cols = c(within_effect, between_effect, cross_effect),
               names_to = "component", values_to = "pct") %>%
  mutate(
    component = case_when(
      component == "within_effect"  ~ "Within-sector",
      component == "between_effect" ~ "Between-sector",
      component == "cross_effect"   ~ "Cross-term"
    ),
    component = factor(component, levels = c("Within-sector", "Between-sector", "Cross-term")),
    is_tha = iso3c == "THA"
  )

# Sort countries by total growth
total_order <- ss_results %>%
  filter(period == "Full (1991-2024)") %>%
  arrange(total_growth) %>%
  pull(short_name)
p42_data <- p42_data %>%
  mutate(short_name = factor(short_name, levels = total_order))

p42 <- ggplot(p42_data, aes(x = short_name, y = pct, fill = component)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_fill_manual(values = c("Within-sector" = "#66C2A5",
                                "Between-sector" = "#FC8D62",
                                "Cross-term" = "#8DA0CB"),
                    name = NULL) +
  coord_flip() +
  labs(
    title    = "Shift-Share Decomposition — Cross-Country Comparison (1991–2024)",
    subtitle = "% of initial aggregate labor productivity; stacked by component",
    x = NULL, y = "Contribution to productivity growth (%)",
    caption  = "Source: World Bank WDI. McMillan & Rodrik (2011) shift-share decomposition."
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank())

save_plot(p42, "plots/42_shift_share_comparison.png")

# =============================================================================
# 7. Save computed metrics
# =============================================================================

cat("\n--- Saving structural transformation metrics ---\n")

output <- panel %>%
  select(iso3c, short_name, country_group, year,
         agriculture_pct_gdp, industry_pct_gdp, services_pct_gdp,
         empl_agri, empl_ind, empl_svc,
         rel_prod_agri, rel_prod_ind, rel_prod_svc,
         prod_gap_ratio)

write_csv(output, "data/structural_transformation.csv")
cat("  Saved: data/structural_transformation.csv\n")

# =============================================================================
# 8. Summary tables
# =============================================================================

cat("\n=== THAILAND STRUCTURAL TRANSFORMATION SUMMARY ===\n")

tha_first <- thailand %>% slice_min(year, n = 1)
tha_last  <- thailand %>% slice_max(year, n = 1)

cat(sprintf("\n  Period: %d – %d\n", tha_first$year, tha_last$year))
cat("\n  Employment shares (start → end):\n")
cat(sprintf("    Agriculture: %.1f%% → %.1f%%\n", tha_first$empl_agri, tha_last$empl_agri))
cat(sprintf("    Industry:    %.1f%% → %.1f%%\n", tha_first$empl_ind, tha_last$empl_ind))
cat(sprintf("    Services:    %.1f%% → %.1f%%\n", tha_first$empl_svc, tha_last$empl_svc))

cat("\n  GDP shares (start → end):\n")
cat(sprintf("    Agriculture: %.1f%% → %.1f%%\n", tha_first$agriculture_pct_gdp, tha_last$agriculture_pct_gdp))
cat(sprintf("    Industry:    %.1f%% → %.1f%%\n", tha_first$industry_pct_gdp, tha_last$industry_pct_gdp))
cat(sprintf("    Services:    %.1f%% → %.1f%%\n", tha_first$services_pct_gdp, tha_last$services_pct_gdp))

cat("\n  Relative productivity (latest):\n")
cat(sprintf("    Agriculture: %.2f  (below average)\n", tha_last$rel_prod_agri))
cat(sprintf("    Industry:    %.2f\n", tha_last$rel_prod_ind))
cat(sprintf("    Services:    %.2f\n", tha_last$rel_prod_svc))

cat(sprintf("\n  Dual-economy gap (non-agri/agri): %.2f (latest)\n", tha_last$prod_gap_ratio))

cat("\n  Shift-share decomposition (full period):\n")
ss_tha_full <- ss_results %>% filter(iso3c == "THA", period == "Full (1991-2024)")
if (nrow(ss_tha_full) > 0) {
  cat(sprintf("    Within-sector:  %+.1f%%\n", ss_tha_full$within_effect))
  cat(sprintf("    Between-sector: %+.1f%%\n", ss_tha_full$between_effect))
  cat(sprintf("    Cross-term:     %+.1f%%\n", ss_tha_full$cross_effect))
  cat(sprintf("    Total:          %+.1f%%\n", ss_tha_full$total_growth))
}

cat("\nDone.\n")
