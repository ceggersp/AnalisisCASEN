# CASEN Analysis: Income, Poverty, and Inequality in Chile (2017-2024)

[![GitHub](https://img.shields.io/badge/GitHub-Repository-blue)](https://github.com/ceggersp/AnalisisCASEN)

Comprehensive analysis of income, poverty, and inequality in Chile using CASEN (Encuesta de Caracterización Socioeconómica Nacional) survey data from 2017 and 2024.

**Repository**: https://github.com/ceggersp/AnalisisCASEN

## Key Findings

- **Poverty reduction**: Total income poverty fell from 9.8% to 4.6%, with transfer efficiency nearly doubling
- **Pro-poor income growth**: Bottom decile grew 26% vs only 7% for top decile (inflation-adjusted)
- **Declining education premium**: University wage premium fell from 125% to 99% at the median

## Poverty Line Definition

This analysis uses the **World Bank $8.3 PPP (2017)** poverty line standard:
- **2017**: $107,347 CLP per capita/month
- **2024**: $152,160 CLP per capita/month

## Income Definitions

Two income measures are analyzed:
- **Total income**: `ytotcorh / numper` (total household income per capita)
- **Autonomous income**: `(ytotcorh - ysubh) / numper` (excluding state transfers/subsidies)

## Household Categories

Households are classified by age composition:
| Category | Definition |
|----------|------------|
| No65 | No members aged 65+ |
| AtLeast1_65 | At least one member aged 65+ |
| Only65 | All members aged 65+ |
| WorkAge | All members aged 18-64 (working age only) |

## Folder Structure

```
CASEN/
├── codes/                  # R scripts
├── raw_data/               # Original CASEN survey data (.dta)
├── harmonized_data/        # Processed datasets with harmonized variables
├── output/                 # Analysis results (Excel, CSV)
├── reports/                # Final reports (PDF, LaTeX, Markdown)
└── README.md
```

## Main Reports

Located in `reports/`:

| File | Description |
|------|-------------|
| `REPORT_Poverty_Comparison_2017_2024.tex` | Main report (English) - LaTeX source |
| `REPORT_Poverty_Comparison_2017_2024_ES.tex` | Main report (Spanish) - LaTeX source |
| `education_premium_*.png` | Education premium graphs (English) |
| `education_premium_*_es.png` | Education premium graphs (Spanish) |

To compile PDF reports:
```bash
pdflatex REPORT_Poverty_Comparison_2017_2024.tex
pdflatex REPORT_Poverty_Comparison_2017_2024_ES.tex
```

## Scripts

### Data Extraction
| Script | Description |
|--------|-------------|
| `codes/extract_variables_2017.R` | Extracts and harmonizes variables from CASEN 2017 |
| `codes/extract_variables_2024.R` | Extracts and harmonizes variables from CASEN 2024 |

### Poverty Analysis
| Script | Description |
|--------|-------------|
| `codes/poverty_analysis.R` | Main poverty analysis script (set `YEAR` parameter) |
| `codes/compare_2017_2024.R` | Compares results between 2017 and 2024 |

### Labor Income Analysis
| Script | Description |
|--------|-------------|
| `codes/labor_income_by_cohort.R` | Calculates hourly labor income by age cohort and education |
| `codes/labor_income_tables_graphs.R` | Generates tables and graphs for the report (EN/ES) |

## How to Run

### 1. Extract Variables (run once per survey year)
```r
source("codes/extract_variables_2017.R")
source("codes/extract_variables_2024.R")
```

### 2. Run Poverty Analysis
Edit `codes/poverty_analysis.R` and set the year:
```r
YEAR <- 2017  # or 2024
```
Then run the script. Results are saved to `output/poverty_comparison_extended_YEAR.xlsx`.

### 3. Compare Years
After running the analysis for both years:
```r
source("codes/compare_2017_2024.R")
```
Results are saved to `output/comparison_2017_2024.xlsx`.

### 4. Labor Income Analysis
```r
source("codes/labor_income_by_cohort.R")      # Calculate hourly income statistics
source("codes/labor_income_tables_graphs.R")  # Generate tables and figures (EN/ES)
```
Results are saved to `output/labor_income_by_cohort.xlsx` and `output/labor_income_tables.xlsx`.
Figures are saved to `reports/education_premium_*.png`.

## Harmonized Variables

The extraction scripts create datasets with these harmonized variables:

| Variable | Description |
|----------|-------------|
| folio | Household ID |
| region | Region code |
| comuna | Comuna code |
| edad | Age |
| numper | Number of persons in household |
| ytotcorh | Total household income (corrected) |
| ysubh | Household subsidies (state transfers) |
| yoprcor | Labor income from main occupation |
| ytrabajocor | Total labor income |
| y2_hrs | Hours worked |
| activ | Activity status (1=Employed, 2=Unemployed, 3=Inactive) |
| o15 | Employment category |
| nivel_educ | Education level (harmonized name) |
| expr | Survey expansion factor (weight) |
| univ | University education dummy (1=Yes, 0=No) |
| year | Survey year |

### Education Coding Note

The `univ` dummy is coded differently due to survey changes:
- **2017**: `nivel_educ >= 7` (codes 7-12 = tertiary/postgrad)
- **2024**: `nivel_educ >= 12` (codes 12-15 = tertiary/postgrad)

## Data Sources

- CASEN 2017: `raw_data/CASEN_2017.dta`
- CASEN 2024: `raw_data/casen_2024.dta` + `raw_data/casen_2024_provincia_comuna.dta`

## Requirements

### R packages
```r
install.packages(c("haven", "dplyr", "tidyr", "openxlsx", "readxl", "ggplot2"))
```

| Package | Purpose |
|---------|---------|
| `haven` | Read/write Stata files |
| `dplyr` | Data manipulation |
| `tidyr` | Data reshaping |
| `openxlsx` | Excel output |
| `readxl` | Read Excel files |
| `ggplot2` | Visualization (graphs) |

### LaTeX (for PDF reports)
A LaTeX distribution (e.g., TeX Live, MiKTeX) with the following packages:
`babel`, `booktabs`, `graphicx`, `hyperref`, `fancyhdr`, `palatino`, `mathpazo`

## Technical Notes

- **Inflation adjustment**: 2017 values adjusted by factor 1.42 to express in 2024 CLP
- **Survey weights**: All statistics use expansion factors (`expr`)
- **Labor income sample**: Employed workers dependent on employer (activ=1, o15 in 3,4,5), ages 26-65
- **Hourly income**: `yoprcor / (y2_hrs * 4.33)` (monthly income / monthly hours)
