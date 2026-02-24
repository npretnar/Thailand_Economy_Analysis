###############################################################################
# 05_labor_by_sector.R
# Sectoral employment shares (agriculture, industry, services) by gender
# Plus labor force composition and participation rates
#
# Data source: World Bank WDI (ILO modeled estimates)
# Coverage: 1991-2024, 17 countries
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

indicators <- c(
  # Sectoral employment shares — total
  empl_agri        = "SL.AGR.EMPL.ZS",
  empl_ind         = "SL.IND.EMPL.ZS",
  empl_svc         = "SL.SRV.EMPL.ZS",
  # Sectoral employment shares — female
  empl_agri_fe     = "SL.AGR.EMPL.FE.ZS",
  empl_ind_fe      = "SL.IND.EMPL.FE.ZS",
  empl_svc_fe      = "SL.SRV.EMPL.FE.ZS",
  # Sectoral employment shares — male
  empl_agri_ma     = "SL.AGR.EMPL.MA.ZS",
  empl_ind_ma      = "SL.IND.EMPL.MA.ZS",
  empl_svc_ma      = "SL.SRV.EMPL.MA.ZS",
  # Labor force totals and participation
  labor_force      = "SL.TLF.TOTL.IN",
  labor_force_fe   = "SL.TLF.TOTL.FE.ZS",
  lfp_rate         = "SL.TLF.CACT.ZS",
  lfp_rate_fe      = "SL.TLF.CACT.FE.ZS",
  lfp_rate_ma      = "SL.TLF.CACT.MA.ZS"
)

countries <- c("THA", "MYS", "IDN", "PHL", "VNM", "SGP", "KHM", "LAO", "MMR", "BRN",
               "USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

asean_codes <- c("THA", "MYS", "IDN", "PHL", "VNM", "SGP", "KHM", "LAO", "MMR", "BRN")
developed_codes <- c("USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

cat("Pulling sectoral employment data...\n")
raw <- WDI(country = countries, indicator = indicators,
           start = 1991, end = 2024, extra = TRUE)
cat("  Raw data:", nrow(raw), "rows\n")

# --- 3. Clean -----------------------------------------------------------------

short_names <- c(
  "THA" = "Thailand", "MYS" = "Malaysia", "IDN" = "Indonesia",
  "PHL" = "Philippines", "VNM" = "Vietnam", "SGP" = "Singapore",
  "KHM" = "Cambodia", "LAO" = "Lao PDR", "MMR" = "Myanmar",
  "BRN" = "Brunei", "USA" = "USA", "JPN" = "Japan",
  "DEU" = "Germany", "GBR" = "UK", "KOR" = "South Korea",
  "AUS" = "Australia", "CHN" = "China"
)

panel <- raw %>%
  mutate(
    country_group = case_when(
      iso3c %in% asean_codes     ~ "ASEAN",
      iso3c %in% developed_codes ~ "Developed",
      TRUE                       ~ "Other"
    ),
    short_name = short_names[iso3c]
  ) %>%
  arrange(country_group, country, year)

# --- 4. Save data -------------------------------------------------------------

write_csv(panel %>% select(iso2c, iso3c, country, short_name, year, country_group,
                           starts_with("empl_"), starts_with("labor_"), starts_with("lfp_")),
          "data/wb_labor_by_sector.csv")
cat("  Saved: data/wb_labor_by_sector.csv\n")

# --- 5. Theme -----------------------------------------------------------------

thai_red   <- "#E3120B"
grey_other <- "#AAAAAA"
blue_dev   <- "#2166AC"

sector_colors <- c(
  "Agriculture" = "#66C2A5",
  "Industry"    = "#FC8D62",
  "Services"    = "#8DA0CB"
)

gender_colors <- c(
  "Female" = "#D95F02",
  "Male"   = "#1B9E77"
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

asean    <- panel %>% filter(country_group == "ASEAN")
thailand <- panel %>% filter(iso3c == "THA")

cat("\nGenerating plots...\n")

# =============================================================================
# PLOT 22: Aggregate sectoral employment shares — Thailand over time (stacked)
# =============================================================================

p22_data <- thailand %>%
  filter(!is.na(empl_agri)) %>%
  select(year, empl_agri, empl_ind, empl_svc) %>%
  pivot_longer(-year, names_to = "sector", values_to = "pct") %>%
  mutate(sector = case_when(
    sector == "empl_agri" ~ "Agriculture",
    sector == "empl_ind"  ~ "Industry",
    sector == "empl_svc"  ~ "Services"
  ),
  sector = factor(sector, levels = c("Services", "Industry", "Agriculture")))

p22 <- ggplot(p22_data, aes(x = year, y = pct, fill = sector)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = sector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Employment by Sector — Thailand",
    subtitle = "% of total employment (both genders), ILO modeled estimates",
    x = NULL, y = "Share of employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai

save_plot(p22, "plots/22_employment_sectors_thailand.png")

# =============================================================================
# PLOT 23: Aggregate sectoral employment — ASEAN line plots (Thailand highlighted)
# =============================================================================

p23_data <- asean %>%
  filter(!is.na(empl_agri)) %>%
  select(iso3c, short_name, year, empl_agri, empl_ind, empl_svc) %>%
  pivot_longer(cols = c(empl_agri, empl_ind, empl_svc),
               names_to = "sector", values_to = "pct") %>%
  mutate(sector = case_when(
    sector == "empl_agri" ~ "Agriculture",
    sector == "empl_ind"  ~ "Industry",
    sector == "empl_svc"  ~ "Services"
  ))

p23_labels <- p23_data %>%
  group_by(iso3c, sector) %>% slice_max(year, n = 1) %>% ungroup()

p23 <- ggplot(p23_data, aes(x = year, y = pct, group = iso3c)) +
  geom_line(data = p23_data %>% filter(iso3c != "THA"),
            color = grey_other, linewidth = 0.5, alpha = 0.6) +
  geom_line(data = p23_data %>% filter(iso3c == "THA"),
            color = thai_red, linewidth = 1.3) +
  geom_text(data = p23_labels %>% filter(iso3c == "THA"),
            aes(label = short_name), color = thai_red,
            hjust = -0.1, size = 3, fontface = "bold") +
  facet_wrap(~sector, scales = "free_y") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Sectoral Employment Shares — ASEAN Countries",
    subtitle = "Thailand highlighted in red; % of total employment",
    x = NULL, y = "Share of employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 12))

save_plot(p23, "plots/23_employment_sectors_asean.png", w = 14, h = 6)

# =============================================================================
# PLOT 24: Cross-country sectoral employment — latest year (stacked bar)
# =============================================================================

p24_data <- panel %>%
  filter(!is.na(empl_agri)) %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup() %>%
  select(iso3c, short_name, country_group, empl_agri, empl_ind, empl_svc) %>%
  pivot_longer(cols = c(empl_agri, empl_ind, empl_svc),
               names_to = "sector", values_to = "pct") %>%
  mutate(
    sector = case_when(
      sector == "empl_agri" ~ "Agriculture",
      sector == "empl_ind"  ~ "Industry",
      sector == "empl_svc"  ~ "Services"
    ),
    sector = factor(sector, levels = c("Agriculture", "Industry", "Services"))
  )

# Sort by services share
svc_order <- p24_data %>%
  filter(sector == "Services") %>% arrange(pct) %>% pull(short_name)
p24_data <- p24_data %>%
  mutate(short_name = factor(short_name, levels = svc_order))

p24 <- ggplot(p24_data, aes(x = short_name, y = pct, fill = sector)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = sector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_flip() +
  labs(
    title    = "Employment by Sector — All Countries (Latest Year)",
    subtitle = "% of total employment, sorted by services share",
    x = NULL, y = "Share of employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank())

save_plot(p24, "plots/24_employment_sectors_bar.png")

# =============================================================================
# PLOT 25: Thailand sectoral employment by GENDER — Female (stacked area)
# =============================================================================

p25_data <- thailand %>%
  filter(!is.na(empl_agri_fe)) %>%
  select(year, empl_agri_fe, empl_ind_fe, empl_svc_fe) %>%
  pivot_longer(-year, names_to = "sector", values_to = "pct") %>%
  mutate(sector = case_when(
    sector == "empl_agri_fe" ~ "Agriculture",
    sector == "empl_ind_fe"  ~ "Industry",
    sector == "empl_svc_fe"  ~ "Services"
  ),
  sector = factor(sector, levels = c("Services", "Industry", "Agriculture")))

p25 <- ggplot(p25_data, aes(x = year, y = pct, fill = sector)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = sector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Female Employment by Sector — Thailand",
    subtitle = "% of female employment, ILO modeled estimates",
    x = NULL, y = "Share of female employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai

save_plot(p25, "plots/25_employment_sectors_thailand_female.png")

# =============================================================================
# PLOT 26: Thailand sectoral employment by GENDER — Male (stacked area)
# =============================================================================

p26_data <- thailand %>%
  filter(!is.na(empl_agri_ma)) %>%
  select(year, empl_agri_ma, empl_ind_ma, empl_svc_ma) %>%
  pivot_longer(-year, names_to = "sector", values_to = "pct") %>%
  mutate(sector = case_when(
    sector == "empl_agri_ma" ~ "Agriculture",
    sector == "empl_ind_ma"  ~ "Industry",
    sector == "empl_svc_ma"  ~ "Services"
  ),
  sector = factor(sector, levels = c("Services", "Industry", "Agriculture")))

p26 <- ggplot(p26_data, aes(x = year, y = pct, fill = sector)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = sector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Male Employment by Sector — Thailand",
    subtitle = "% of male employment, ILO modeled estimates",
    x = NULL, y = "Share of male employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai

save_plot(p26, "plots/26_employment_sectors_thailand_male.png")

# =============================================================================
# PLOT 27: Thailand sectoral employment — Female vs Male side-by-side lines
# =============================================================================

p27_data <- thailand %>%
  filter(!is.na(empl_agri_fe)) %>%
  select(year, empl_agri_fe, empl_ind_fe, empl_svc_fe,
         empl_agri_ma, empl_ind_ma, empl_svc_ma) %>%
  pivot_longer(-year, names_to = "var", values_to = "pct") %>%
  mutate(
    gender = ifelse(grepl("_fe$", var), "Female", "Male"),
    sector = case_when(
      grepl("agri", var) ~ "Agriculture",
      grepl("ind", var)  ~ "Industry",
      grepl("svc", var)  ~ "Services"
    )
  )

p27 <- ggplot(p27_data, aes(x = year, y = pct, color = gender, linetype = gender)) +
  geom_line(linewidth = 1.1) +
  facet_wrap(~sector, scales = "free_y") +
  scale_color_manual(values = gender_colors, name = NULL) +
  scale_linetype_manual(values = c("Female" = "solid", "Male" = "dashed"), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = "Sectoral Employment by Gender — Thailand",
    subtitle = "Female vs male employment shares within each sector",
    x = NULL, y = "Share of gender-specific employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 12))

save_plot(p27, "plots/27_employment_gender_comparison_thailand.png", w = 14, h = 6)

# =============================================================================
# PLOT 28: Labor force participation rate by gender — ASEAN
# =============================================================================

p28_data <- asean %>%
  filter(!is.na(lfp_rate_fe)) %>%
  select(iso3c, short_name, year, lfp_rate_fe, lfp_rate_ma) %>%
  pivot_longer(cols = c(lfp_rate_fe, lfp_rate_ma),
               names_to = "gender", values_to = "rate") %>%
  mutate(gender = ifelse(gender == "lfp_rate_fe", "Female", "Male"))

p28_labels <- p28_data %>%
  group_by(iso3c, gender) %>% slice_max(year, n = 1) %>% ungroup()

p28 <- ggplot(p28_data, aes(x = year, y = rate, group = iso3c)) +
  geom_line(data = p28_data %>% filter(iso3c != "THA"),
            color = grey_other, linewidth = 0.5, alpha = 0.6) +
  geom_line(data = p28_data %>% filter(iso3c == "THA"),
            color = thai_red, linewidth = 1.3) +
  geom_text(data = p28_labels %>% filter(iso3c == "THA"),
            aes(label = "Thailand"), color = thai_red,
            hjust = -0.1, size = 3, fontface = "bold") +
  facet_wrap(~gender) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  coord_cartesian(xlim = c(1990, 2030)) +
  labs(
    title    = "Labor Force Participation Rate by Gender — ASEAN Countries",
    subtitle = "Thailand highlighted in red; % of working-age population (15+)",
    x = NULL, y = "Labor force participation rate (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 12))

save_plot(p28, "plots/28_lfp_rate_gender_asean.png", w = 12, h = 6)

# =============================================================================
# PLOT 29: Female share of labor force — all countries bar chart (latest year)
# =============================================================================

p29_data <- panel %>%
  filter(!is.na(labor_force_fe)) %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup() %>%
  mutate(
    bar_fill = case_when(
      iso3c == "THA" ~ "Thailand",
      country_group == "ASEAN" ~ "ASEAN",
      TRUE ~ "Developed"
    ),
    short_name = fct_reorder(short_name, labor_force_fe)
  )

p29 <- ggplot(p29_data, aes(x = short_name, y = labor_force_fe, fill = bar_fill)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", labor_force_fe)),
            hjust = -0.1, size = 3.2) +
  scale_fill_manual(
    values = c("Thailand" = thai_red, "ASEAN" = "#FC8D62", "Developed" = "#8DA0CB"),
    name = NULL
  ) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(
    title    = "Female Share of Labor Force — All Countries",
    subtitle = "Dashed line = 50% (parity)",
    x = NULL, y = "Female share of labor force (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai +
  theme(panel.grid.major.y = element_blank())

save_plot(p29, "plots/29_female_labor_share_bar.png")

# =============================================================================
# PLOT 30-32: Gender composition WITHIN each sector — Thailand over time
# i.e., what share of agriculture workers are female vs male?
# =============================================================================

# Compute actual worker counts by gender and sector
gender_within <- thailand %>%
  filter(!is.na(empl_agri_fe), !is.na(labor_force), !is.na(labor_force_fe)) %>%
  mutate(
    lf_female = labor_force * labor_force_fe / 100,
    lf_male   = labor_force * (1 - labor_force_fe / 100),
    # Workers in each sector by gender
    workers_agri_fe = lf_female * empl_agri_fe / 100,
    workers_agri_ma = lf_male   * empl_agri_ma / 100,
    workers_ind_fe  = lf_female * empl_ind_fe  / 100,
    workers_ind_ma  = lf_male   * empl_ind_ma  / 100,
    workers_svc_fe  = lf_female * empl_svc_fe  / 100,
    workers_svc_ma  = lf_male   * empl_svc_ma  / 100,
    # Female share within each sector
    fe_share_agri = 100 * workers_agri_fe / (workers_agri_fe + workers_agri_ma),
    fe_share_ind  = 100 * workers_ind_fe  / (workers_ind_fe  + workers_ind_ma),
    fe_share_svc  = 100 * workers_svc_fe  / (workers_svc_fe  + workers_svc_ma)
  )

# --- Plot 30: Stacked area — gender within Agriculture (Thailand)
p30_data <- gender_within %>%
  select(year, workers_agri_fe, workers_agri_ma) %>%
  mutate(across(starts_with("workers"), ~ . / 1e6)) %>%
  pivot_longer(-year, names_to = "gender", values_to = "millions") %>%
  mutate(gender = ifelse(grepl("_fe$", gender), "Female", "Male"),
         gender = factor(gender, levels = c("Male", "Female")))

p30 <- ggplot(p30_data, aes(x = year, y = millions, fill = gender)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = gender_colors, name = NULL) +
  scale_y_continuous(labels = label_comma(suffix = "M")) +
  labs(
    title    = "Agriculture Employment by Gender — Thailand",
    subtitle = "Millions of workers",
    x = NULL, y = "Workers (millions)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai

save_plot(p30, "plots/30_agri_employment_by_gender_thailand.png")

# --- Plot 31: Stacked area — gender within Industry (Thailand)
p31_data <- gender_within %>%
  select(year, workers_ind_fe, workers_ind_ma) %>%
  mutate(across(starts_with("workers"), ~ . / 1e6)) %>%
  pivot_longer(-year, names_to = "gender", values_to = "millions") %>%
  mutate(gender = ifelse(grepl("_fe$", gender), "Female", "Male"),
         gender = factor(gender, levels = c("Male", "Female")))

p31 <- ggplot(p31_data, aes(x = year, y = millions, fill = gender)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = gender_colors, name = NULL) +
  scale_y_continuous(labels = label_comma(suffix = "M")) +
  labs(
    title    = "Industry Employment by Gender — Thailand",
    subtitle = "Millions of workers",
    x = NULL, y = "Workers (millions)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai

save_plot(p31, "plots/31_industry_employment_by_gender_thailand.png")

# --- Plot 32: Stacked area — gender within Services (Thailand)
p32_data <- gender_within %>%
  select(year, workers_svc_fe, workers_svc_ma) %>%
  mutate(across(starts_with("workers"), ~ . / 1e6)) %>%
  pivot_longer(-year, names_to = "gender", values_to = "millions") %>%
  mutate(gender = ifelse(grepl("_fe$", gender), "Female", "Male"),
         gender = factor(gender, levels = c("Male", "Female")))

p32 <- ggplot(p32_data, aes(x = year, y = millions, fill = gender)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = gender_colors, name = NULL) +
  scale_y_continuous(labels = label_comma(suffix = "M")) +
  labs(
    title    = "Services Employment by Gender — Thailand",
    subtitle = "Millions of workers",
    x = NULL, y = "Workers (millions)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai

save_plot(p32, "plots/32_services_employment_by_gender_thailand.png")

# =============================================================================
# PLOT 33: Female share within each sector — Thailand over time (line plot)
# =============================================================================

p33_data <- gender_within %>%
  select(year, fe_share_agri, fe_share_ind, fe_share_svc) %>%
  pivot_longer(-year, names_to = "sector", values_to = "fe_pct") %>%
  mutate(sector = case_when(
    sector == "fe_share_agri" ~ "Agriculture",
    sector == "fe_share_ind"  ~ "Industry",
    sector == "fe_share_svc"  ~ "Services"
  ))

p33 <- ggplot(p33_data, aes(x = year, y = fe_pct, color = sector)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = sector_colors, name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  annotate("text", x = 1992, y = 51, label = "Parity (50%)",
           size = 3, color = "grey50", hjust = 0) +
  labs(
    title    = "Female Share of Employment Within Each Sector — Thailand",
    subtitle = "What fraction of workers in each sector are women?",
    x = NULL, y = "Female share of sector employment (%)",
    caption  = "Source: World Bank WDI (ILO estimates). Computed from gender-specific shares and labor force composition."
  ) +
  theme_thai

save_plot(p33, "plots/33_female_share_within_sectors_thailand.png")

# =============================================================================
# PLOT 34: Female share within each sector — cross-country bar (latest year)
# =============================================================================

gender_within_all <- panel %>%
  filter(!is.na(empl_agri_fe), !is.na(labor_force), !is.na(labor_force_fe)) %>%
  group_by(iso3c) %>% slice_max(year, n = 1) %>% ungroup() %>%
  mutate(
    lf_female = labor_force * labor_force_fe / 100,
    lf_male   = labor_force * (1 - labor_force_fe / 100),
    workers_agri_fe = lf_female * empl_agri_fe / 100,
    workers_agri_ma = lf_male   * empl_agri_ma / 100,
    workers_ind_fe  = lf_female * empl_ind_fe  / 100,
    workers_ind_ma  = lf_male   * empl_ind_ma  / 100,
    workers_svc_fe  = lf_female * empl_svc_fe  / 100,
    workers_svc_ma  = lf_male   * empl_svc_ma  / 100,
    fe_share_agri = 100 * workers_agri_fe / (workers_agri_fe + workers_agri_ma),
    fe_share_ind  = 100 * workers_ind_fe  / (workers_ind_fe  + workers_ind_ma),
    fe_share_svc  = 100 * workers_svc_fe  / (workers_svc_fe  + workers_svc_ma)
  )

p34_data <- gender_within_all %>%
  select(iso3c, short_name, country_group, fe_share_agri, fe_share_ind, fe_share_svc) %>%
  pivot_longer(cols = starts_with("fe_share"), names_to = "sector", values_to = "fe_pct") %>%
  mutate(
    sector = case_when(
      sector == "fe_share_agri" ~ "Agriculture",
      sector == "fe_share_ind"  ~ "Industry",
      sector == "fe_share_svc"  ~ "Services"
    ),
    is_tha = iso3c == "THA"
  )

# Sort by services female share
svc_fe_order <- p34_data %>%
  filter(sector == "Services") %>% arrange(fe_pct) %>% pull(short_name)
p34_data <- p34_data %>%
  mutate(short_name = factor(short_name, levels = svc_fe_order))

p34 <- ggplot(p34_data, aes(x = short_name, y = fe_pct)) +
  geom_col(aes(fill = ifelse(iso3c == "THA", "Thailand", "Other")),
           width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  scale_fill_manual(values = c("Thailand" = thai_red, "Other" = "#8DA0CB")) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  facet_wrap(~sector) +
  coord_flip() +
  labs(
    title    = "Female Share of Sector Employment — All Countries (Latest Year)",
    subtitle = "Dashed line = 50% parity; Thailand highlighted in red",
    x = NULL, y = "Female share (%)",
    caption  = "Source: World Bank WDI (ILO estimates)"
  ) +
  theme_thai +
  theme(strip.text = element_text(face = "bold", size = 12),
        panel.grid.major.y = element_blank())

save_plot(p34, "plots/34_female_share_within_sectors_comparison.png", w = 14, h = 7)

# --- 6. Summary ---------------------------------------------------------------

cat("\n=== THAILAND LABOR SUMMARY (latest year) ===\n")
tha_latest <- thailand %>% filter(!is.na(empl_agri)) %>% slice_max(year, n = 1)
cat(sprintf("  Year: %d\n", tha_latest$year))
cat(sprintf("  Labor force: %.1f million (%.1f%% female)\n",
    tha_latest$labor_force / 1e6, tha_latest$labor_force_fe))
cat(sprintf("  LFP rate: %.1f%% total | %.1f%% female | %.1f%% male\n",
    tha_latest$lfp_rate, tha_latest$lfp_rate_fe, tha_latest$lfp_rate_ma))
cat("\n  Sectoral employment (Total | Female | Male):\n")
cat(sprintf("    Agriculture: %5.1f%% | %5.1f%% | %5.1f%%\n",
    tha_latest$empl_agri, tha_latest$empl_agri_fe, tha_latest$empl_agri_ma))
cat(sprintf("    Industry:    %5.1f%% | %5.1f%% | %5.1f%%\n",
    tha_latest$empl_ind, tha_latest$empl_ind_fe, tha_latest$empl_ind_ma))
cat(sprintf("    Services:    %5.1f%% | %5.1f%% | %5.1f%%\n",
    tha_latest$empl_svc, tha_latest$empl_svc_fe, tha_latest$empl_svc_ma))

cat("\nDone.\n")
