###############################################################################
# 03_manufacturing_subsectors.R
# Manufacturing sub-sector composition: Thailand vs ASEAN and developed economies
#
# Data source: World Bank WDI (originally sourced from UNIDO)
# Sub-sectors: Food/Bev/Tobacco, Textiles/Clothing, Chemicals,
#              Machinery/Transport, Other manufacturing
#              + Medium/high-tech (cross-cutting indicator)
# Coverage: 1990–2022 for most countries
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

# --- 2. Pull manufacturing sub-sector data -----------------------------------

# 5 mutually exclusive sub-sectors (sum to ~100% of mfg value added)
indicators_subsector <- c(
  food_bev_tobacco  = "NV.MNF.FBTO.ZS.UN",
  textiles_clothing = "NV.MNF.TXTL.ZS.UN",
  chemicals         = "NV.MNF.CHEM.ZS.UN",
  machinery_transp  = "NV.MNF.MTRN.ZS.UN",
  other_mfg         = "NV.MNF.OTHR.ZS.UN"
)

# Cross-cutting indicator (overlaps with chemicals + machinery)
indicators_tech <- c(
  med_hitech_pct = "NV.MNF.TECH.ZS.UN"
)

countries <- c("THA", "MYS", "IDN", "PHL", "VNM", "SGP", "KHM", "LAO", "MMR", "BRN",
               "USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

asean_codes <- c("THA", "MYS", "IDN", "PHL", "VNM", "SGP", "KHM", "LAO", "MMR", "BRN")
developed_codes <- c("USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

cat("Pulling manufacturing sub-sector data...\n")

raw_sub <- WDI(
  country   = countries,
  indicator = c(indicators_subsector, indicators_tech),
  start     = 1990,
  end       = 2024,
  extra     = TRUE
)

cat("  Raw data:", nrow(raw_sub), "rows\n")

# --- 3. Clean ----------------------------------------------------------------

short_names <- c(
  "THA" = "Thailand", "MYS" = "Malaysia", "IDN" = "Indonesia",
  "PHL" = "Philippines", "VNM" = "Vietnam", "SGP" = "Singapore",
  "KHM" = "Cambodia", "LAO" = "Lao PDR", "MMR" = "Myanmar",
  "BRN" = "Brunei", "USA" = "USA", "JPN" = "Japan",
  "DEU" = "Germany", "GBR" = "UK", "KOR" = "South Korea",
  "AUS" = "Australia", "CHN" = "China"
)

mfg_panel <- raw_sub %>%
  mutate(
    country_group = case_when(
      iso3c %in% asean_codes     ~ "ASEAN",
      iso3c %in% developed_codes ~ "Developed",
      TRUE                       ~ "Other"
    ),
    short_name = short_names[iso3c]
  ) %>%
  select(iso2c, iso3c, country, short_name, year, country_group,
         food_bev_tobacco, textiles_clothing, chemicals,
         machinery_transp, other_mfg, med_hitech_pct)

# Long format for the 5 mutually exclusive sub-sectors
mfg_long <- mfg_panel %>%
  pivot_longer(
    cols      = c(food_bev_tobacco, textiles_clothing, chemicals,
                  machinery_transp, other_mfg),
    names_to  = "subsector",
    values_to = "pct_of_mfg"
  ) %>%
  mutate(
    subsector_label = case_when(
      subsector == "food_bev_tobacco"  ~ "Food, Beverages & Tobacco",
      subsector == "textiles_clothing" ~ "Textiles & Clothing",
      subsector == "chemicals"         ~ "Chemicals",
      subsector == "machinery_transp"  ~ "Machinery & Transport",
      subsector == "other_mfg"         ~ "Other Manufacturing"
    ),
    subsector_label = factor(subsector_label, levels = c(
      "Other Manufacturing",
      "Textiles & Clothing",
      "Chemicals",
      "Machinery & Transport",
      "Food, Beverages & Tobacco"
    ))
  )

# --- 4. Save data -------------------------------------------------------------

write_csv(mfg_panel, "data/wb_manufacturing_subsectors.csv")
write_csv(mfg_long,  "data/wb_manufacturing_subsectors_long.csv")

cat("  Saved: data/wb_manufacturing_subsectors.csv (", nrow(mfg_panel), "rows)\n")
cat("  Saved: data/wb_manufacturing_subsectors_long.csv (", nrow(mfg_long), "rows)\n")

# --- 5. Theme and constants ---------------------------------------------------

thai_red   <- "#E3120B"
grey_other <- "#AAAAAA"

subsector_colors <- c(
  "Food, Beverages & Tobacco" = "#66C2A5",
  "Textiles & Clothing"       = "#FC8D62",
  "Chemicals"                 = "#8DA0CB",
  "Machinery & Transport"     = "#E78AC3",
  "Other Manufacturing"       = "#A6D854"
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

cat("\nGenerating plots...\n")

# =============================================================================
# PLOT 9: Thailand manufacturing sub-sector composition over time (stacked area)
# =============================================================================

p9_data <- mfg_long %>%
  filter(iso3c == "THA", !is.na(pct_of_mfg))

# Only keep years where all 5 sub-sectors have data
p9_complete <- p9_data %>%
  group_by(year) %>%
  filter(sum(!is.na(pct_of_mfg)) == 5) %>%
  ungroup()

p9 <- ggplot(p9_complete, aes(x = year, y = pct_of_mfg, fill = subsector_label)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = subsector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Manufacturing Sub-Sector Composition — Thailand",
    subtitle = "% of total manufacturing value added",
    x = NULL, y = "Share of manufacturing (%)",
    caption  = "Source: World Bank WDI (UNIDO data)"
  ) +
  theme_thai +
  guides(fill = guide_legend(nrow = 2))

save_plot(p9, "plots/09_mfg_subsectors_thailand.png")

# =============================================================================
# PLOT 10: Thailand manufacturing sub-sectors as individual lines over time
# =============================================================================

p10_data <- mfg_long %>%
  filter(iso3c == "THA", !is.na(pct_of_mfg))

p10_labels <- p10_data %>%
  group_by(subsector_label) %>%
  slice_max(year, n = 1) %>%
  ungroup()

p10 <- ggplot(p10_data, aes(x = year, y = pct_of_mfg,
                             color = subsector_label, group = subsector_label)) +
  geom_line(linewidth = 1.1) +
  geom_text(data = p10_labels, aes(label = subsector_label),
            hjust = -0.05, size = 3, show.legend = FALSE) +
  scale_color_manual(values = subsector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_cartesian(xlim = c(1990, 2030)) +
  labs(
    title    = "Thailand Manufacturing Sub-Sectors Over Time",
    subtitle = "Each sub-sector as % of total manufacturing value added",
    x = NULL, y = "Share of manufacturing (%)",
    caption  = "Source: World Bank WDI (UNIDO data)"
  ) +
  theme_thai +
  theme(legend.position = "none")

save_plot(p10, "plots/10_mfg_subsectors_thailand_lines.png")

# =============================================================================
# PLOT 11: Cross-country manufacturing composition — latest year (stacked bar)
# =============================================================================

# Get latest year with complete sub-sector data per country
p11_data <- mfg_long %>%
  filter(!is.na(pct_of_mfg)) %>%
  group_by(iso3c, year) %>%
  filter(n() == 5) %>%   # all 5 sub-sectors present
  ungroup() %>%
  group_by(iso3c, subsector_label) %>%
  slice_max(year, n = 1) %>%
  ungroup()

# Sort by machinery & transport share (a proxy for industrial sophistication)
machinery_order <- p11_data %>%
  filter(subsector_label == "Machinery & Transport") %>%
  arrange(pct_of_mfg) %>%
  pull(short_name)

p11_data <- p11_data %>%
  mutate(short_name = factor(short_name, levels = machinery_order))

# Drop countries with no sub-sector data (e.g. Brunei)
p11_data <- p11_data %>% filter(!is.na(short_name))

p11 <- ggplot(p11_data, aes(x = short_name, y = pct_of_mfg, fill = subsector_label)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = subsector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(
    title    = "Manufacturing Sub-Sector Composition — Cross-Country Comparison",
    subtitle = "% of manufacturing value added (latest year), sorted by Machinery & Transport share",
    x = NULL, y = "Share of manufacturing (%)",
    caption  = "Source: World Bank WDI (UNIDO data)"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

save_plot(p11, "plots/11_mfg_subsectors_comparison.png")

# =============================================================================
# PLOT 12: ASEAN manufacturing composition — latest year (stacked bar)
# =============================================================================

p12_data <- p11_data %>% filter(iso3c %in% asean_codes)

# Re-sort for ASEAN only
machinery_order_asean <- p12_data %>%
  filter(subsector_label == "Machinery & Transport") %>%
  arrange(pct_of_mfg) %>%
  pull(short_name)

p12_data <- p12_data %>%
  mutate(short_name = factor(short_name, levels = machinery_order_asean))

p12 <- ggplot(p12_data, aes(x = short_name, y = pct_of_mfg, fill = subsector_label)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = subsector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(
    title    = "Manufacturing Sub-Sector Composition — ASEAN Countries",
    subtitle = "% of manufacturing value added (latest year), sorted by Machinery & Transport share",
    x = NULL, y = "Share of manufacturing (%)",
    caption  = "Source: World Bank WDI (UNIDO data)"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank()) +
  guides(fill = guide_legend(nrow = 2))

save_plot(p12, "plots/12_mfg_subsectors_asean.png")

# =============================================================================
# PLOT 13: Medium & high-tech manufacturing share over time
# =============================================================================

p13_data <- mfg_panel %>%
  filter(!is.na(med_hitech_pct), country_group == "ASEAN")

p13_labels <- p13_data %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup()

p13 <- ggplot(p13_data, aes(x = year, y = med_hitech_pct, group = iso3c)) +
  geom_line(data = p13_data %>% filter(iso3c != "THA"),
            aes(color = "Other ASEAN"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p13_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p13_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_color_manual(values = c("Thailand" = thai_red, "Other ASEAN" = grey_other),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2028)) +
  labs(
    title    = "Medium & High-Tech Manufacturing Share — ASEAN Countries",
    subtitle = "% of total manufacturing value added",
    x = NULL, y = "Med/high-tech share of manufacturing (%)",
    caption  = "Source: World Bank WDI (UNIDO data)"
  ) +
  theme_thai

save_plot(p13, "plots/13_med_hitech_asean.png")

# =============================================================================
# PLOT 14: Medium & high-tech — Thailand vs developed economies
# =============================================================================

p14_data <- mfg_panel %>%
  filter(!is.na(med_hitech_pct),
         iso3c %in% c("THA", developed_codes))

p14_labels <- p14_data %>%
  group_by(iso3c) %>%
  slice_max(year, n = 1) %>%
  ungroup()

blue_dev <- "#2166AC"

p14 <- ggplot(p14_data, aes(x = year, y = med_hitech_pct, group = iso3c)) +
  geom_line(data = p14_data %>% filter(iso3c != "THA"),
            aes(color = "Developed"), linewidth = 0.6, alpha = 0.7) +
  geom_line(data = p14_data %>% filter(iso3c == "THA"),
            aes(color = "Thailand"), linewidth = 1.4) +
  geom_text(data = p14_labels, aes(label = short_name),
            hjust = -0.1, size = 3, check_overlap = TRUE) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_color_manual(values = c("Thailand" = thai_red, "Developed" = blue_dev),
                     name = NULL) +
  coord_cartesian(xlim = c(1990, 2028)) +
  labs(
    title    = "Medium & High-Tech Manufacturing — Thailand vs Developed Economies",
    subtitle = "% of total manufacturing value added",
    x = NULL, y = "Med/high-tech share of manufacturing (%)",
    caption  = "Source: World Bank WDI (UNIDO data)"
  ) +
  theme_thai

save_plot(p14, "plots/14_med_hitech_developed.png")

# --- 6. Summary --------------------------------------------------------------

cat("\n=== THAILAND MANUFACTURING SUB-SECTORS (latest available year) ===\n")
latest_tha <- mfg_panel %>%
  filter(iso3c == "THA", !is.na(food_bev_tobacco)) %>%
  slice_max(year, n = 1)

if (nrow(latest_tha) > 0) {
  cat(sprintf("  Year: %d\n", latest_tha$year))
  cat(sprintf("  Food, Beverages & Tobacco: %.1f%%\n", latest_tha$food_bev_tobacco))
  cat(sprintf("  Textiles & Clothing:       %.1f%%\n", latest_tha$textiles_clothing))
  cat(sprintf("  Chemicals:                 %.1f%%\n", latest_tha$chemicals))
  cat(sprintf("  Machinery & Transport:     %.1f%%\n", latest_tha$machinery_transp))
  cat(sprintf("  Other Manufacturing:       %.1f%%\n", latest_tha$other_mfg))
}

latest_tech <- mfg_panel %>%
  filter(iso3c == "THA", !is.na(med_hitech_pct)) %>%
  slice_max(year, n = 1)

if (nrow(latest_tech) > 0) {
  cat(sprintf("  Med/High-Tech Share:       %.1f%% (%d)\n",
              latest_tech$med_hitech_pct, latest_tech$year))
}

cat("\nDone.\n")
