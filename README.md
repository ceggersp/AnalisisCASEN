# CASEN Analysis: Income, Poverty, and Inequality in Chile (2017-2024)

[![GitHub](https://img.shields.io/badge/GitHub-Repository-blue)](https://github.com/ceggersp/AnalisisCASEN)

Comprehensive analysis of income, poverty, and inequality in Chile using CASEN (Encuesta de Caracterización Socioeconómica Nacional) survey data from 2017 and 2024.

**Repository**: https://github.com/ceggersp/AnalisisCASEN

## Key Findings

- **Poverty reduction**: Total income poverty fell from 9.8% to 4.6%, with transfer efficiency improving from 60.8% to 76.2% of autonomous poverty eliminated
- **Near-elimination of elderly poverty**: State transfers eliminate 99.7% of autonomous poverty among elderly-only households, reducing total income poverty to just 0.12%
- **Pro-poor income growth**: Bottom decile grew 51% in total income vs only 19% for top decile (inflation-adjusted)
- **Declining education premium**: University wage premium fell from 126% to 99% at the median, with non-university workers seeing 21% wage growth vs only 7% for university graduates

## Poverty Line Definition

This analysis uses the **World Bank $8.3 PPP (2017)** poverty line standard:
- **2017**: $107,347 CLP per capita/month
- **2024**: $152,160 CLP per capita/month

## Income Definitions

Two income measures are analyzed:
- **Total income**: `ytotcorh / numper` (corrected household income per capita, including all sources)
- **Autonomous income**: `yautcorh / numper` (includes only labor income, self-employment, and contributory pensions—excludes all state transfers)
- **State transfers**: Non-contributory pensions (PGU, PBS) + subsidies + other government transfers

## Household Categories

Households are classified by age composition:
| Category | Definition |
|----------|------------|
| No65 | Households without members aged 65+ |
| AtLeast1_65 | Households with at least one member aged 65+ |
| Only65 | Households where all members are aged 65+ |
| WorkAge | Households with only members aged 24-64 (working age only) |

## Folder Structure

```
CASEN/
├── codes/                  # R scripts
├── raw_data/               # Original CASEN survey data (.dta)
├── harmonized_data/        # Processed datasets with harmonized variables
├── output/
│   ├── tables/             # Excel output files
│   └── figures/            # Generated PNG figures
├── reports/                # Final reports (LaTeX source files)
└── README.md
```

## Main Reports

Located in `reports/`:

| File | Description |
|------|-------------|
| `REPORT_Poverty_Comparison_2017_2024.tex` | Main report (English) - LaTeX source |
| `REPORT_Poverty_Comparison_2017_2024_ES.tex` | Main report (Spanish) - LaTeX source |

### Figures (in `output/figures/`)

| File | Description |
|------|-------------|
| `education_premium_median.png` | Education premium at median (English) |
| `education_premium_p25.png` | Education premium at P25 (English) |
| `education_premium_p75.png` | Education premium at P75 (English) |
| `education_premium_p90.png` | Education premium at P90 (English) |
| `income_by_education_2017.png` | Income comparison by education, 2017 (English) |
| `income_by_education_2024.png` | Income comparison by education, 2024 (English) |
| `*_es.png` | Spanish versions of all figures |

### Tables (in `output/tables/`)

| File | Description |
|------|-------------|
| `poverty_comparison_extended_2017.xlsx` | Poverty analysis for 2017 |
| `poverty_comparison_extended_2024.xlsx` | Poverty analysis for 2024 |
| `comparison_2017_2024.xlsx` | Year-over-year comparison |
| `labor_income_by_cohort.xlsx` | Hourly income by age cohort |
| `labor_income_tables.xlsx` | Summary tables with education premium |

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
Then run the script. Results are saved to `output/tables/poverty_comparison_extended_YEAR.xlsx`.

### 3. Compare Years
After running the analysis for both years:
```r
source("codes/compare_2017_2024.R")
```
Results are saved to `output/tables/comparison_2017_2024.xlsx`.

### 4. Labor Income Analysis
```r
source("codes/labor_income_by_cohort.R")      # Calculate hourly income statistics
source("codes/labor_income_tables_graphs.R")  # Generate tables and figures (EN/ES)
```
Results are saved to `output/tables/labor_income_by_cohort.xlsx` and `output/tables/labor_income_tables.xlsx`.

Generated figures (saved to `output/figures/`):
- Education premium by age cohort (P25, median, P75, P90)
- Income comparison by education level (2017 vs 2024)
- All figures in English and Spanish versions

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
- **Income deciles**: Defined based on autonomous income distribution
- **Education premium**: Calculated as `100 * (Univ_income / NoUniv_income - 1)`

## Report Highlights

### Poverty and Transfers
| Metric | 2017 | 2024 | Change |
|--------|------|------|--------|
| Total income poverty | 9.80% | 4.59% | -5.21 pp |
| Autonomous income poverty | 25.01% | 19.32% | -5.69 pp |
| Transfer efficiency | 60.8% | 76.2% | +15.4 pp |

### Education Premium (Median Hourly Income)
| Metric | 2017 | 2024 | Change |
|--------|------|------|--------|
| No University income | 2,584 CLP/hr | 3,125 CLP/hr | +21% |
| University income | 5,818 CLP/hr | 6,215 CLP/hr | +7% |
| Education premium | 125% | 99% | -26 pp |

---

*Report generated: January 2026*
*Data: CASEN 2017 and CASEN 2024, Ministerio de Desarrollo Social y Familia, Chile*
