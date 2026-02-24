###############################################################################
# 04_services_agriculture_subsectors.R
# Agriculture and services sub-sector indicators:
#   Agriculture: Crop & livestock production indices, value added per worker
#   Services: Tourism receipts (key Thailand sub-sector), total services VA
#
# Note: Granular services sub-sector breakdown (finance, trade, transport) is
# not available from the WDI API. For that level of detail, UNIDO INDSTAT
# data must be downloaded manually from https://stat.unido.org/
###############################################################################

# --- 1. Setup ----------------------------------------------------------------

required_packages <- c("WDI", "tidyverse", "scales")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(WDI)
library(tidyverse)
library(scales)

# --- 2. Pull data -------------------------------------------------------------

# Agriculture indicators
indicators_agri <- c(
  crop_index         = "AG.PRD.CROP.XD",
  livestock_index    = "AG.PRD.LVSK.XD",
  agri_va_usd        = "NV.AGR.TOTL.CD",
  agri_va_per_worker = "NV.AGR.EMPL.KD"
)

# Services / tourism indicators
indicators_svc <- c(
  svc_va_usd            = "NV.SRV.TOTL.CD",
  tourism_receipts_usd  = "ST.INT.RCPT.CD",
  tourism_pct_exports   = "ST.INT.RCPT.XP.ZS",
  gdp_usd               = "NY.GDP.MKTP.CD"
)

countries <- c("THA", "MYS", "IDN", "PHL", "VNM", "SGP", "KHM", "LAO", "MMR", "BRN",
               "USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

asean_codes <- c("THA", "MYS", "IDN", "PHL", "VNM", "SGP", "KHM", "LAO", "MMR", "BRN")
developed_codes <- c("USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

cat("Pulling agriculture indicators...\n")
raw_agri <- WDI(country = countries, indicator = indicators_agri,
                start = 1990, end = 2024, extra = TRUE)

cat("Pulling services/tourism indicators...\n")
raw_svc <- WDI(country = countries, indicator = indicators_svc,
               start = 1990, end = 2024)

# Merge
panel <- raw_agri %>%
  left_join(
    raw_svc %>% select(iso2c, year, names(indicators_svc)),
    by = c("iso2c", "year")
  ) %>%
  mutate(
    country_group = case_when(
      iso3c %in% asean_codes     ~ "ASEAN",
      iso3c %in% developed_codes ~ "Developed",
      TRUE                       ~ "Other"
    )
  )

short_names <- c(
  "THA" = "Thailand", "MYS" = "Malaysia", "IDN" = "Indonesia",
  "PHL" = "Philippines", "VNM" = "Vietnam", "SGP" = "Singapore",
  "KHM" = "Cambodia", "LAO" = "Lao PDR", "MMR" = "Myanmar",
  "BRN" = "Brunei", "USA" = "USA", "JPN" = "Japan",
  "DEU" = "Germany", "GBR" = "UK", "KOR" = "South Korea",
  "AUS" = "Australia", "CHN" = "China"
)

panel <- panel %>% mutate(short_name = short_names[iso3c])

# Compute tourism as % of GDP
panel <- panel %>%
  mutate(tourism_pct_gdp = 100 * tourism_receipts_usd / gdp_usd)

cat("  Panel:", nrow(panel), "rows\n")

# --- 3. Save data -------------------------------------------------------------

write_csv(panel, "data/wb_agri_services_subsectors.csv")
cat("  Saved: data/wb_agri_services_subsectors.csv\n")

# --- 4. Theme -----------------------------------------------------------------

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

asean <- panel %>% filter(country_group == "ASEAN")
thailand <- panel %>% filter(iso3c == "THA")

cat("\nGenerating plots...\n")

# =============================================================================
# PLOT 15: Crop Production Index — ASEAN (Thailand highlighted)
# =============================================================================

p15_data <- asean %>% filter(!is.na(crop_index))

p15_labels <- p15_data %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p15 <- ggplot(p15_data, aes(x = year, y = crop_index, group = iso3c)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey60") +
  geom_line(data = p15_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p15_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p15_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2028)) +
  annotate("text", x = 1991, y = 102, label = "Base = 100 (2014-2016)",
           size = 3, color = "grey50", hjust = 0) +
  labs(
    title    = "Crop Production Index — ASEAN Countries",
    subtitle = "FAO index (2014-2016 = 100)",
    x = NULL, y = "Crop production index",
    caption  = "Source: World Bank WDI (FAO data)"
  ) +
  theme_thai

save_plot(p15, "plots/15_crop_index_asean.png")

# =============================================================================
# PLOT 16: Livestock Production Index — ASEAN
# =============================================================================

p16_data <- asean %>% filter(!is.na(livestock_index))

p16_labels <- p16_data %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p16 <- ggplot(p16_data, aes(x = year, y = livestock_index, group = iso3c)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey60") +
  geom_line(data = p16_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p16_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p16_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2028)) +
  annotate("text", x = 1991, y = 102, label = "Base = 100 (2014-2016)",
           size = 3, color = "grey50", hjust = 0) +
  labs(
    title    = "Livestock Production Index — ASEAN Countries",
    subtitle = "FAO index (2014-2016 = 100)",
    x = NULL, y = "Livestock production index",
    caption  = "Source: World Bank WDI (FAO data)"
  ) +
  theme_thai

save_plot(p16, "plots/16_livestock_index_asean.png")

# =============================================================================
# PLOT 17: Agriculture Value Added Per Worker — ASEAN
# =============================================================================

p17_data <- asean %>% filter(!is.na(agri_va_per_worker))

p17_labels <- p17_data %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p17 <- ggplot(p17_data, aes(x = year, y = agri_va_per_worker, group = iso3c)) +
  geom_line(data = p17_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p17_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p17_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2030)) +
  labs(
    title    = "Agriculture Value Added Per Worker — ASEAN Countries",
    subtitle = "Constant 2015 US$",
    x = NULL, y = "Value added per worker (constant 2015 US$)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p17, "plots/17_agri_productivity_asean.png")

# =============================================================================
# PLOT 18: Agriculture Value Added Per Worker — Thailand vs Developed
# =============================================================================

p18_data <- bind_rows(thailand, panel %>% filter(country_group == "Developed")) %>%
  filter(!is.na(agri_va_per_worker))

p18_labels <- p18_data %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p18 <- ggplot(p18_data, aes(x = year, y = agri_va_per_worker, group = iso3c)) +
  geom_line(data = p18_data %>% filter(iso3c != "THA"),
            aes(color = "Developed"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p18_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p18_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_log10(labels = label_dollar()) +
  scale_color_manual(values = c("Thailand" = thai_red, "Developed" = blue_dev),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2030)) +
  labs(
    title    = "Agriculture Value Added Per Worker — Thailand vs Developed Economies",
    subtitle = "Constant 2015 US$, log scale",
    x = NULL, y = "Value added per worker (constant 2015 US$)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p18, "plots/18_agri_productivity_developed.png")

# =============================================================================
# PLOT 19: Tourism Receipts as % of GDP — ASEAN Countries
# =============================================================================

p19_data <- asean %>% filter(!is.na(tourism_pct_gdp))

p19_labels <- p19_data %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p19 <- ggplot(p19_data, aes(x = year, y = tourism_pct_gdp, group = iso3c)) +
  geom_line(data = p19_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p19_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p19_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1995, 2026)) +
  labs(
    title    = "International Tourism Receipts as % of GDP — ASEAN Countries",
    subtitle = "Tourism is a key services sub-sector for Thailand",
    x = NULL, y = "Tourism receipts (% of GDP)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p19, "plots/19_tourism_pct_gdp_asean.png")

# =============================================================================
# PLOT 20: Tourism Receipts — latest year bar chart, ASEAN + developed
# =============================================================================

p20_data <- panel %>%
  filter(!is.na(tourism_receipts_usd)) %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup() %>%
  mutate(
    tourism_billions = tourism_receipts_usd / 1e9,
    bar_fill = case_when(
      iso3c == "THA" ~ "Thailand",
      country_group == "ASEAN" ~ "ASEAN",
      TRUE ~ "Developed"
    ),
    short_name = fct_reorder(short_name, tourism_billions)
  )

p20 <- ggplot(p20_data, aes(x = short_name, y = tourism_billions, fill = bar_fill)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("$%.1fB", tourism_billions)),
            hjust = -0.1, size = 3.2) +
  scale_fill_manual(
    values = c("Thailand" = thai_red, "ASEAN" = "#FC8D62", "Developed" = "#8DA0CB"),
    name = NULL
  ) +
  scale_y_continuous(labels = label_dollar(suffix = "B"),
                     expand = expansion(mult = c(0, 0.25))) +
  coord_flip() +
  labs(
    title    = "International Tourism Receipts — All Countries",
    subtitle = paste0("Latest available year (data through 2020)"),
    x = NULL, y = "Tourism receipts (billions USD)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank())

save_plot(p20, "plots/20_tourism_receipts_bar.png")

# =============================================================================
# PLOT 21: Tourism Receipts as % of Exports — ASEAN
# =============================================================================

p21_data <- asean %>% filter(!is.na(tourism_pct_exports))

p21_labels <- p21_data %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup()

p21 <- ggplot(p21_data, aes(x = year, y = tourism_pct_exports, group = iso3c)) +
  geom_line(data = p21_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p21_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p21_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1995, 2026)) +
  labs(
    title    = "Tourism Receipts as % of Total Exports — ASEAN Countries",
    subtitle = "Shows tourism dependence relative to total export revenue",
    x = NULL, y = "Tourism receipts (% of exports)",
    caption  = "Source: World Bank WDI"
  ) +
  theme_thai

save_plot(p21, "plots/21_tourism_pct_exports_asean.png")

# --- 5. Summary ---------------------------------------------------------------

cat("\n=== THAILAND AGRICULTURE & SERVICES SUB-SECTOR SUMMARY ===\n")
latest_tha <- thailand %>%
  filter(!is.na(crop_index)) %>%
  slice_max(year, n = 1)

if (nrow(latest_tha) > 0) {
  cat(sprintf("  Crop Production Index: %.1f (%d) [base 100 = 2014-2016]\n",
              latest_tha$crop_index, latest_tha$year))
  cat(sprintf("  Livestock Production Index: %.1f (%d)\n",
              latest_tha$livestock_index, latest_tha$year))
}

latest_worker <- thailand %>%
  filter(!is.na(agri_va_per_worker)) %>%
  slice_max(year, n = 1)
if (nrow(latest_worker) > 0) {
  cat(sprintf("  Agri Value Added/Worker: $%.0f (%d, constant 2015 US$)\n",
              latest_worker$agri_va_per_worker, latest_worker$year))
}

latest_tourism <- thailand %>%
  filter(!is.na(tourism_receipts_usd)) %>%
  slice_max(year, n = 1)
if (nrow(latest_tourism) > 0) {
  cat(sprintf("  Tourism Receipts: $%.1f billion (%d)\n",
              latest_tourism$tourism_receipts_usd / 1e9, latest_tourism$year))
  cat(sprintf("  Tourism as %% of GDP: %.1f%%\n", latest_tourism$tourism_pct_gdp))
  cat(sprintf("  Tourism as %% of Exports: %.1f%%\n", latest_tourism$tourism_pct_exports))
}

cat("\nDone.\n")
