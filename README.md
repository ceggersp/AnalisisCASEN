# CASEN Poverty Analysis (2017-2024)

Analysis of poverty rates in Chile using CASEN (Encuesta de Caracterización Socioeconómica Nacional) survey data from 2017 and 2024.

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
- **poverty_analysis_report.pdf** - Main analysis report with tables and findings
- **poverty_analysis_report.tex** - LaTeX source for the PDF report

## Scripts

### Data Extraction
| Script | Description |
|--------|-------------|
| `codes/extract_variables_2017.R` | Extracts and harmonizes variables from CASEN 2017 |
| `codes/extract_variables_2024.R` | Extracts and harmonizes variables from CASEN 2024 |

### Analysis
| Script | Description |
|--------|-------------|
| `codes/poverty_analysis.R` | Main poverty analysis script (set `YEAR` parameter) |
| `codes/compare_2017_2024.R` | Compares results between 2017 and 2024 |

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

R packages:
- `haven` - Read/write Stata files
- `dplyr` - Data manipulation
- `openxlsx` - Excel output
- `readxl` - Read Excel files
- `tidyr` - Data reshaping
