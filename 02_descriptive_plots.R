###############################################################################
# 02_descriptive_plots.R
# Descriptive plots comparing Thailand to ASEAN peers and developed economies
#
# Reads: data/wb_full_panel.csv, data/wb_sectoral.csv
# Outputs: 8 PNG files in plots/
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

# --- 2. Load data -------------------------------------------------------------

panel    <- read_csv("data/wb_full_panel.csv", show_col_types = FALSE)
sectoral <- read_csv("data/wb_sectoral.csv", show_col_types = FALSE)

asean <- panel %>% filter(country_group == "ASEAN")
developed <- panel %>% filter(country_group == "Developed")
thailand <- panel %>% filter(iso3c == "THA")

# --- 3. Theme and constants ---------------------------------------------------

thai_red   <- "#E3120B"
grey_other <- "#AAAAAA"
blue_dev   <- "#2166AC"

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

# Short country labels for direct labeling
short_names <- c(
  "THA" = "Thailand", "MYS" = "Malaysia", "IDN" = "Indonesia",
  "PHL" = "Philippines", "VNM" = "Vietnam", "SGP" = "Singapore",
  "KHM" = "Cambodia", "LAO" = "Lao PDR", "MMR" = "Myanmar",
  "BRN" = "Brunei", "USA" = "USA", "JPN" = "Japan",
  "DEU" = "Germany", "GBR" = "UK", "KOR" = "South Korea",
  "AUS" = "Australia", "CHN" = "China"
)

panel    <- panel %>% mutate(short_name = short_names[iso3c])
asean    <- asean %>% mutate(short_name = short_names[iso3c])
developed <- developed %>% mutate(short_name = short_names[iso3c])
thailand <- thailand %>% mutate(short_name = "Thailand")

cat("Generating plots...\n")

# =============================================================================
# PLOT 1: GDP over time — ASEAN countries (log scale)
# =============================================================================

p1_data <- asean %>%
  filter(!is.na(gdp_current_usd)) %>%
  mutate(gdp_billions = gdp_current_usd / 1e9)

# Endpoint labels
p1_labels <- p1_data %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup()

p1 <- ggplot(p1_data, aes(x = year, y = gdp_billions, group = iso3c)) +
  geom_line(data = p1_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p1_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p1_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_log10(labels = label_dollar(suffix = "B"),
                breaks = c(0.1, 1, 10, 100, 1000)) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1960, 2030)) +
  labs(
    title    = "GDP Over Time — ASEAN Countries",
    subtitle = "Current US$, log scale",
    x = NULL, y = "GDP (billions USD)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p1, "plots/01_gdp_asean.png")

# =============================================================================
# PLOT 2: GDP per capita (PPP) over time — ASEAN
# =============================================================================

p2_data <- asean %>%
  filter(!is.na(gdp_pc_ppp_current))

p2_labels <- p2_data %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup()

p2 <- ggplot(p2_data, aes(x = year, y = gdp_pc_ppp_current, group = iso3c)) +
  geom_line(data = p2_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p2_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p2_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2030)) +
  labs(
    title    = "GDP Per Capita (PPP) Over Time — ASEAN Countries",
    subtitle = "Current international $",
    x = NULL, y = "GDP per capita (PPP)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p2, "plots/02_gdp_pc_ppp_asean.png")

# =============================================================================
# PLOT 3: GDP per capita (PPP) — Thailand vs developed economies
# =============================================================================

p3_data <- bind_rows(thailand, developed) %>%
  filter(!is.na(gdp_pc_ppp_current))

p3_labels <- p3_data %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup()

p3 <- ggplot(p3_data, aes(x = year, y = gdp_pc_ppp_current, group = iso3c)) +
  geom_line(data = p3_data %>% filter(iso3c != "THA"),
            aes(color = "Developed"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p3_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p3_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_manual(values = c("Thailand" = thai_red, "Developed" = blue_dev),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2030)) +
  labs(
    title    = "GDP Per Capita (PPP) — Thailand vs Developed Economies",
    subtitle = "Current international $",
    x = NULL, y = "GDP per capita (PPP)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p3, "plots/03_gdp_pc_ppp_developed.png")

# =============================================================================
# PLOT 4: GDP per capita (PPP) — latest year bar chart, all countries
# =============================================================================

p4_data <- panel %>%
  filter(!is.na(gdp_pc_ppp_current)) %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup() %>%
  mutate(
    bar_fill = case_when(
      iso3c == "THA" ~ "Thailand",
      country_group == "ASEAN" ~ "ASEAN",
      TRUE ~ "Developed"
    ),
    short_name = fct_reorder(short_name, gdp_pc_ppp_current)
  )

p4 <- ggplot(p4_data, aes(x = short_name, y = gdp_pc_ppp_current, fill = bar_fill)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label_dollar()(round(gdp_pc_ppp_current))),
            hjust = -0.1, size = 3.2) +
  scale_fill_manual(
    values = c("Thailand" = thai_red, "ASEAN" = "#FC8D62", "Developed" = "#8DA0CB"),
    name = NULL
  ) +
  scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.25))) +
  coord_flip() +
  labs(
    title    = "GDP Per Capita (PPP) — All Countries",
    subtitle = paste0("Latest available year (", p4_data$year[1], ")"),
    x = NULL, y = "GDP per capita, PPP (current international $)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank())

save_plot(p4, "plots/04_gdp_pc_ppp_bar.png")

# =============================================================================
# PLOT 5: Real GDP growth rate — Thailand vs ASEAN range
# =============================================================================

asean_growth_band <- asean %>%
  filter(!is.na(gdp_growth_annual)) %>%
  group_by(year) %>%
  summarise(
    median_growth = median(gdp_growth_annual, na.rm = TRUE),
    q25 = quantile(gdp_growth_annual, 0.25, na.rm = TRUE),
    q75 = quantile(gdp_growth_annual, 0.75, na.rm = TRUE),
    min_growth = min(gdp_growth_annual, na.rm = TRUE),
    max_growth = max(gdp_growth_annual, na.rm = TRUE),
    .groups = "drop"
  )

tha_growth <- thailand %>% filter(!is.na(gdp_growth_annual))

# Key crisis years
crises <- tibble(
  year  = c(1997, 2008, 2020),
  label = c("Asian Financial\nCrisis", "Global Financial\nCrisis", "COVID-19"),
  y     = c(-12, -5, -8)
)

p5 <- ggplot() +
  geom_ribbon(data = asean_growth_band,
              aes(x = year, ymin = min_growth, ymax = max_growth),
              fill = grey_other, alpha = 0.2) +
  geom_ribbon(data = asean_growth_band,
              aes(x = year, ymin = q25, ymax = q75),
              fill = grey_other, alpha = 0.3) +
  geom_line(data = asean_growth_band,
            aes(x = year, y = median_growth, linetype = "ASEAN median"),
            color = grey_other, linewidth = 0.8) +
  geom_line(data = tha_growth,
            aes(x = year, y = gdp_growth_annual, linetype = "Thailand"),
            color = thai_red, linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_text(data = crises, aes(x = year, y = y, label = label),
            size = 2.8, color = "grey40", fontface = "italic") +
  scale_linetype_manual(values = c("Thailand" = "solid", "ASEAN median" = "dashed"),
                        name = NULL) +
  labs(
    title    = "Real GDP Growth Rate — Thailand vs ASEAN Range",
    subtitle = "Shaded area: ASEAN min/max (light) and IQR (dark)",
    x = NULL, y = "Annual GDP growth (%)",
    caption  = "Source: World Bank WDI. Growth computed from constant 2015 US$ GDP."
  ) +
  theme_thai +
  guides(linetype = guide_legend(override.aes = list(
    color = c(grey_other, thai_red), linewidth = c(0.8, 1.2)
  )))

save_plot(p5, "plots/05_gdp_growth.png")

# =============================================================================
# PLOT 6: Population over time — ASEAN countries
# =============================================================================

p6_data <- asean %>%
  filter(!is.na(population)) %>%
  mutate(pop_millions = population / 1e6)

p6_labels <- p6_data %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup()

p6 <- ggplot(p6_data, aes(x = year, y = pop_millions, group = iso3c)) +
  geom_line(data = p6_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p6_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p6_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_comma(suffix = "M")) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1960, 2030)) +
  labs(
    title    = "Population Over Time — ASEAN Countries",
    subtitle = "Total population (millions)",
    x = NULL, y = "Population (millions)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p6, "plots/06_population_asean.png")

# =============================================================================
# PLOT 7: Sectoral composition — Thailand over time (stacked area)
# =============================================================================

# Only keep years where all 3 main sectors have data (services starts ~1990)
tha_complete_years <- sectoral %>%
  filter(iso3c == "THA", sector != "Manufacturing") %>%
  group_by(year) %>%
  filter(all(!is.na(pct_of_gdp))) %>%
  ungroup()

p7_data <- tha_complete_years

# Order sectors logically
p7_data <- p7_data %>%
  mutate(sector = factor(sector, levels = c("Services", "Industry", "Agriculture")))

p7 <- ggplot(p7_data, aes(x = year, y = pct_of_gdp, fill = sector)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(
    values = c("Agriculture" = "#66C2A5", "Industry" = "#FC8D62", "Services" = "#8DA0CB"),
    name = NULL
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Sectoral Composition of GDP — Thailand",
    subtitle = "Value added as % of GDP",
    x = NULL, y = "Share of GDP (%)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p7, "plots/07_sectoral_thailand.png")

# =============================================================================
# PLOT 8: Sectoral composition — cross-country comparison (latest year)
# =============================================================================

p8_data <- sectoral %>%
  filter(sector != "Manufacturing", !is.na(pct_of_gdp)) %>%
  group_by(iso3c, sector) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Sort countries by services share
services_order <- p8_data %>%
  filter(sector == "Services") %>%
  arrange(pct_of_gdp) %>%
  pull(country)

p8_data <- p8_data %>%
  mutate(
    country = factor(country, levels = services_order),
    sector  = factor(sector, levels = c("Agriculture", "Industry", "Services")),
    is_tha  = iso3c == "THA"
  )

p8 <- ggplot(p8_data, aes(x = country, y = pct_of_gdp, fill = sector)) +
  geom_col(width = 0.7) +
  # Bold label for Thailand
  scale_x_discrete(labels = function(x) ifelse(x == "Thailand",
    expression(bold("Thailand")), x)) +
  scale_fill_manual(
    values = c("Agriculture" = "#66C2A5", "Industry" = "#FC8D62", "Services" = "#8DA0CB"),
    name = NULL
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(
    title    = "Sectoral Composition of GDP — All Countries",
    subtitle = "Value added as % of GDP (latest available year), sorted by services share",
    x = NULL, y = "Share of GDP (%)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank())

save_plot(p8, "plots/08_sectoral_comparison.png")

cat("\nAll plots generated.\n")
