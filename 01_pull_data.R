###############################################################################
# 01_pull_data.R
# Pull World Bank Development Indicators for Thailand Economy Analysis
#
# Data source: World Bank WDI via the WDI R package
# Coverage: 1960-2024, 17 countries (ASEAN-10 + 7 developed comparators)
# Indicators: GDP, GDP per capita, population, sectoral composition, growth
###############################################################################

# --- 1. Setup ----------------------------------------------------------------

# Install packages if not already available
required_packages <- c("WDI", "tidyverse")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

library(WDI)
library(tidyverse)

# --- 2. Define parameters ----------------------------------------------------

# Country groups
asean_codes <- c("THA", "MYS", "IDN", "PHL", "VNM",
                 "SGP", "KHM", "LAO", "MMR", "BRN")

developed_codes <- c("USA", "JPN", "DEU", "GBR", "KOR", "AUS", "CHN")

all_countries <- c(asean_codes, developed_codes)

# Pull indicators in two batches to avoid API timeouts
# Batch 1: Core macro + population
indicators_macro <- c(
  gdp_current_usd        = "NY.GDP.MKTP.CD",
  gdp_constant_2015_usd  = "NY.GDP.MKTP.KD",
  gdp_pc_current_usd     = "NY.GDP.PCAP.CD",
  gdp_pc_constant_2015   = "NY.GDP.PCAP.KD",
  gdp_ppp_current        = "NY.GDP.MKTP.PP.CD",
  gdp_pc_ppp_current     = "NY.GDP.PCAP.PP.CD",
  population             = "SP.POP.TOTL"
)

# Batch 2: Sectoral composition
indicators_sectoral <- c(
  agriculture_pct_gdp    = "NV.AGR.TOTL.ZS",
  industry_pct_gdp       = "NV.IND.TOTL.ZS",
  services_pct_gdp       = "NV.SRV.TOTL.ZS",
  manufacturing_pct_gdp  = "NV.IND.MANF.ZS"
)

start_year <- 1960
end_year   <- 2024

# --- 3. Pull data from World Bank API (in batches) ---------------------------

cat("Pulling World Bank data for", length(all_countries), "countries,",
    start_year, "-", end_year, "\n")

cat("  Batch 1: Macro indicators (7 indicators)...\n")
raw_macro <- WDI(
  country   = all_countries,
  indicator = indicators_macro,
  start     = start_year,
  end       = end_year,
  extra     = TRUE
)
cat("    ->", nrow(raw_macro), "rows\n")

cat("  Batch 2: Sectoral indicators (4 indicators)...\n")
raw_sectoral <- WDI(
  country   = all_countries,
  indicator = indicators_sectoral,
  start     = start_year,
  end       = end_year
)
cat("    ->", nrow(raw_sectoral), "rows\n")

# Merge batches
raw_data <- raw_macro %>%
  left_join(
    raw_sectoral %>% select(iso2c, year, names(indicators_sectoral)),
    by = c("iso2c", "year")
  )

cat("  Merged:", nrow(raw_data), "rows x", ncol(raw_data), "columns\n")

# --- 4. Clean, compute growth rates, reshape ---------------------------------

# Add country group classification and compute growth rates from constant GDP
panel <- raw_data %>%
  mutate(
    country_group = case_when(
      iso3c %in% asean_codes     ~ "ASEAN",
      iso3c %in% developed_codes ~ "Developed",
      TRUE                       ~ "Other"
    ),
    is_thailand = iso3c == "THA"
  ) %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  mutate(
    gdp_growth_annual    = 100 * (gdp_constant_2015_usd / lag(gdp_constant_2015_usd) - 1),
    gdp_pc_growth_annual = 100 * (gdp_pc_constant_2015 / lag(gdp_pc_constant_2015) - 1)
  ) %>%
  ungroup() %>%
  arrange(country_group, country, year) %>%
  select(
    # Identifiers
    iso2c, iso3c, country, year, country_group, is_thailand,
    # Core macro
    gdp_current_usd, gdp_constant_2015_usd,
    gdp_pc_current_usd, gdp_pc_constant_2015,
    gdp_ppp_current, gdp_pc_ppp_current,
    population,
    # Sectoral
    agriculture_pct_gdp, industry_pct_gdp, services_pct_gdp,
    manufacturing_pct_gdp,
    # Growth (computed)
    gdp_growth_annual, gdp_pc_growth_annual,
    # World Bank metadata
    region, income, lending
  )

cat("  Cleaned panel:", nrow(panel), "rows\n")

# --- 5. Create derived datasets ----------------------------------------------

# Thailand-only subset
thailand <- panel %>% filter(iso3c == "THA")

# Sectoral composition in long format (for easy plotting)
sectoral <- panel %>%
  select(iso3c, country, year, country_group,
         agriculture_pct_gdp, industry_pct_gdp, services_pct_gdp,
         manufacturing_pct_gdp) %>%
  pivot_longer(
    cols      = c(agriculture_pct_gdp, industry_pct_gdp,
                  services_pct_gdp, manufacturing_pct_gdp),
    names_to  = "sector",
    values_to = "pct_of_gdp"
  ) %>%
  mutate(
    sector = str_replace(sector, "_pct_gdp", "") %>%
      str_replace_all("_", " ") %>%
      str_to_title()
  )

# --- 6. Save to CSV ----------------------------------------------------------

write_csv(panel,    "data/wb_full_panel.csv")
write_csv(thailand, "data/wb_thailand_detail.csv")
write_csv(sectoral, "data/wb_sectoral.csv")

cat("\nFiles saved:\n")
cat("  data/wb_full_panel.csv      -", nrow(panel), "rows\n")
cat("  data/wb_thailand_detail.csv -", nrow(thailand), "rows\n")
cat("  data/wb_sectoral.csv        -", nrow(sectoral), "rows\n")

# --- 7. Summary statistics ----------------------------------------------------

cat("\n=== DATA COVERAGE SUMMARY ===\n\n")

# Year range per country
coverage <- panel %>%
  group_by(country_group, country, iso3c) %>%
  summarise(
    year_min = min(year),
    year_max = max(year),
    n_years  = n(),
    gdp_pct_available = round(100 * mean(!is.na(gdp_current_usd)), 1),
    sectoral_pct_available = round(100 * mean(!is.na(agriculture_pct_gdp)), 1),
    .groups = "drop"
  ) %>%
  arrange(country_group, country)

print(as.data.frame(coverage), row.names = FALSE)

# Thailand latest values
cat("\n=== THAILAND LATEST VALUES ===\n")
latest_tha <- thailand %>%
  filter(!is.na(gdp_current_usd)) %>%
  slice_max(year, n = 1)

if (nrow(latest_tha) > 0) {
  cat(sprintf("  Year: %d\n", latest_tha$year))
  cat(sprintf("  GDP (current US$): $%.1f billion\n",
              latest_tha$gdp_current_usd / 1e9))
  cat(sprintf("  GDP per capita (current US$): $%.0f\n",
              latest_tha$gdp_pc_current_usd))
  cat(sprintf("  GDP per capita PPP (current intl $): $%.0f\n",
              latest_tha$gdp_pc_ppp_current))
  cat(sprintf("  Population: %.1f million\n",
              latest_tha$population / 1e6))
  cat(sprintf("  Agriculture: %.1f%% | Industry: %.1f%% | Services: %.1f%%\n",
              latest_tha$agriculture_pct_gdp,
              latest_tha$industry_pct_gdp,
              latest_tha$services_pct_gdp))
  cat(sprintf("  GDP growth: %.1f%%\n", latest_tha$gdp_growth_annual))
}

cat("\nDone.\n")
