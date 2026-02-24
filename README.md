# Thailand Economy Analysis

Descriptive data analysis of the Thai economy using World Bank Development Indicators (WDI), comparing Thailand to ASEAN peers and developed economies worldwide.

## Overview

This project pulls macroeconomic data from the World Bank API and produces publication-quality visualizations covering:

- **GDP and GDP per capita** (nominal, constant 2015 USD, and PPP-adjusted)
- **Population** trends
- **Real GDP growth** rates
- **Sectoral composition** of GDP (agriculture, industry, services)
- **Manufacturing sub-sectors** (food/beverages/tobacco, textiles, chemicals, machinery/transport, medium/high-tech)
- **Agriculture** productivity and production indices (crops, livestock)
- **Tourism** receipts and dependence

### Country Coverage

| Group | Countries |
|-------|-----------|
| **ASEAN (10)** | Thailand, Malaysia, Indonesia, Philippines, Vietnam, Singapore, Cambodia, Lao PDR, Myanmar, Brunei |
| **Developed (7)** | USA, Japan, Germany, UK, South Korea, Australia, China |

### Time Range

1960–2024 (varies by indicator; most have complete coverage from 1990 onward).

## Thailand at a Glance (2024)

| Indicator | Value |
|-----------|-------|
| GDP (current US$) | $526.5 billion |
| GDP per capita (current US$) | $7,347 |
| GDP per capita (PPP) | $24,712 |
| Population | 71.7 million |
| Agriculture share of GDP | 8.7% |
| Industry share of GDP | 32.1% |
| Services share of GDP | 59.2% |
| Real GDP growth | 2.5% |

## Selected Plots

### GDP Per Capita (PPP) — All Countries
![GDP Per Capita Bar Chart](plots/04_gdp_pc_ppp_bar.png)

### Real GDP Growth — Thailand vs ASEAN Range
![GDP Growth](plots/05_gdp_growth.png)

### Sectoral Composition — Thailand Over Time
![Sectoral Thailand](plots/07_sectoral_thailand.png)

### Manufacturing Sub-Sectors — Cross-Country Comparison
![Manufacturing Comparison](plots/11_mfg_subsectors_comparison.png)

### Tourism Receipts as % of GDP — ASEAN
![Tourism](plots/19_tourism_pct_gdp_asean.png)

## Repository Structure

```
Thailand_Economy_Analysis/
├── 01_pull_data.R                      # Pull core WDI data (GDP, population, sectoral)
├── 02_descriptive_plots.R              # 8 plots: GDP, growth, population, sectors
├── 03_manufacturing_subsectors.R       # 6 plots: manufacturing sub-sector breakdown
├── 04_services_agriculture_subsectors.R # 7 plots: agriculture, tourism
├── data/
│   ├── wb_full_panel.csv               # Full panel (17 countries × 65 years × 13 indicators)
│   ├── wb_thailand_detail.csv          # Thailand-only subset
│   ├── wb_sectoral.csv                 # Sectoral composition (long format)
│   ├── wb_manufacturing_subsectors.csv # Manufacturing sub-sectors (wide)
│   ├── wb_manufacturing_subsectors_long.csv # Manufacturing sub-sectors (long)
│   └── wb_agri_services_subsectors.csv # Agriculture & tourism indicators
├── plots/                              # 21 PNG figures (300 DPI)
└── README.md
```

## Data Sources

All data is pulled programmatically from the [World Bank Open Data](https://data.worldbank.org/) API via the [`WDI`](https://cran.r-project.org/package=WDI) R package.

| Category | WDI Indicator Codes |
|----------|-------------------|
| GDP (current US$) | `NY.GDP.MKTP.CD` |
| GDP (constant 2015 US$) | `NY.GDP.MKTP.KD` |
| GDP per capita (current US$) | `NY.GDP.PCAP.CD` |
| GDP per capita (constant 2015 US$) | `NY.GDP.PCAP.KD` |
| GDP, PPP (current intl $) | `NY.GDP.MKTP.PP.CD` |
| GDP per capita, PPP | `NY.GDP.PCAP.PP.CD` |
| Population | `SP.POP.TOTL` |
| Agriculture (% of GDP) | `NV.AGR.TOTL.ZS` |
| Industry (% of GDP) | `NV.IND.TOTL.ZS` |
| Services (% of GDP) | `NV.SRV.TOTL.ZS` |
| Manufacturing (% of GDP) | `NV.IND.MANF.ZS` |
| Food/bev/tobacco (% of mfg) | `NV.MNF.FBTO.ZS.UN` |
| Textiles/clothing (% of mfg) | `NV.MNF.TXTL.ZS.UN` |
| Chemicals (% of mfg) | `NV.MNF.CHEM.ZS.UN` |
| Machinery/transport (% of mfg) | `NV.MNF.MTRN.ZS.UN` |
| Med/high-tech mfg (% of mfg) | `NV.MNF.TECH.ZS.UN` |
| Crop production index | `AG.PRD.CROP.XD` |
| Livestock production index | `AG.PRD.LVSK.XD` |
| Agri value added per worker | `NV.AGR.EMPL.KD` |
| Tourism receipts (US$) | `ST.INT.RCPT.CD` |
| Tourism (% of exports) | `ST.INT.RCPT.XP.ZS` |

Manufacturing sub-sector data is originally sourced from [UNIDO](https://stat.unido.org/) and distributed through WDI.

## Requirements

- **R** (tested with R 4.x)
- **CRAN packages**: `WDI`, `tidyverse`, `scales`

Packages are installed automatically if missing when running any script.

## Usage

Run scripts in order:

```r
source("01_pull_data.R")                       # ~30 seconds (API calls)
source("02_descriptive_plots.R")               # ~10 seconds
source("03_manufacturing_subsectors.R")        # ~15 seconds
source("04_services_agriculture_subsectors.R") # ~15 seconds
```

Or from the command line:

```bash
Rscript 01_pull_data.R
Rscript 02_descriptive_plots.R
Rscript 03_manufacturing_subsectors.R
Rscript 04_services_agriculture_subsectors.R
```

## Notes

- **Services sub-sector granularity**: The WDI API no longer serves detailed services sub-sector breakdowns (finance, trade, transport). For that level of detail, [UNIDO INDSTAT](https://stat.unido.org/) data must be downloaded manually.
- **Tourism data**: Coverage ends at 2020 due to WDI reporting lags and the COVID-19 disruption.
- **Growth rates**: Computed from constant 2015 US$ GDP rather than pulled directly, to avoid API timeout issues with the pre-computed WDI growth indicators.

## License

Data is sourced from the World Bank under the [Creative Commons Attribution 4.0 International License (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/).
