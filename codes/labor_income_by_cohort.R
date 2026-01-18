# CASEN - Labor Income per Hour by Age Cohort and Education
# Compares 2017 and 2024 for employed workers (dependent on employer)
# ===========================================================================

library(haven)
library(dplyr)
library(tidyr)
library(openxlsx)

setwd("C:/Users/CARLOSEG/Documents/CASEN")

# ===========================================================================
# PARAMETERS
# ===========================================================================

# Inflation adjustment factor: 2017 -> 2024
# Based on CPI ratio (adjust if needed)
INFLATION_FACTOR_2017_TO_2024 <- 1.42

# Age cohort breaks
AGE_BREAKS <- c(26, 31, 36, 41, 46, 51, 56, 61, 66)
AGE_LABELS <- c("26-30", "31-35", "36-40", "41-46", "46-50", "51-55", "56-60", "61-65")

# Output file
OUTPUT_FILE <- "output/labor_income_by_cohort.xlsx"

# ===========================================================================
# FUNCTIONS
# ===========================================================================

# Weighted quantile function
weighted_quantile <- function(x, w, probs) {
  # Remove NAs
  valid <- !is.na(x) & !is.na(w)
  x <- x[valid]
  w <- w[valid]

  # Sort by x
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]

  # Cumulative weights (normalized)
  cum_w <- cumsum(w) / sum(w)

  # Find quantiles
  sapply(probs, function(p) {
    idx <- which(cum_w >= p)[1]
    if (is.na(idx)) return(NA)
    x[idx]
  })
}

calculate_hourly_income <- function(data, year, inflation_factor = 1) {

  cat("\n--- Processing year:", year, "---\n")

  # Filter: employed workers dependent on employer
  # activ == 1 (Employed) AND o15 in 3,4,5 (Employee categories)
  employed <- data %>%
    filter(
      activ == 1,                    # Employed
      o15 %in% c(3, 4, 5),           # Dependent employee (not employer/self-employed)
      edad >= 26 & edad <= 65,       # Working age cohorts
      !is.na(yoprcor),               # Has labor income
      !is.na(y2_hrs),                # Has hours data
      y2_hrs > 0                     # Positive hours
    )

  cat("Employed workers (dependent, age 26-65):", nrow(employed), "\n")

  # Calculate hourly income (monthly income / (hours per week * 4.33 weeks))
  employed <- employed %>%
    mutate(
      hourly_income = yoprcor / (y2_hrs * 4.33),
      # Adjust 2017 income to 2024 prices
      hourly_income_adj = hourly_income * inflation_factor,
      # Create age cohorts
      age_cohort = cut(edad,
                       breaks = AGE_BREAKS,
                       labels = AGE_LABELS,
                       right = FALSE,
                       include.lowest = TRUE),
      # Education category
      educ_category = case_when(
        univ == 1 ~ "University",
        univ == 0 ~ "No University",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_cohort), !is.na(educ_category))

  cat("After filtering (valid cohorts & education):", nrow(employed), "\n")

  # Function to calculate all statistics for a group
  calc_stats <- function(df) {
    quantiles <- weighted_quantile(df$hourly_income_adj, df$expr, c(0.10, 0.25, 0.50, 0.75, 0.90))
    tibble(
      n_obs = nrow(df),
      n_weighted = sum(df$expr),
      mean = weighted.mean(df$hourly_income_adj, df$expr, na.rm = TRUE),
      p10 = quantiles[1],
      p25 = quantiles[2],
      median = quantiles[3],
      p75 = quantiles[4],
      p90 = quantiles[5]
    )
  }

  # Calculate statistics by cohort - TOTAL
  total_by_cohort <- employed %>%
    group_by(age_cohort) %>%
    group_modify(~ calc_stats(.x)) %>%
    ungroup() %>%
    mutate(educ_category = "Total")

  # Calculate statistics by cohort and education
  by_cohort_educ <- employed %>%
    group_by(age_cohort, educ_category) %>%
    group_modify(~ calc_stats(.x)) %>%
    ungroup()

  # Combine results
  results <- bind_rows(total_by_cohort, by_cohort_educ) %>%
    mutate(year = year) %>%
    select(year, age_cohort, educ_category, n_obs, n_weighted, mean, p10, p25, median, p75, p90)

  return(results)
}

# ===========================================================================
# LOAD DATA
# ===========================================================================

cat("Loading harmonized datasets...\n")

casen_2017 <- read_dta("harmonized_data/casen_subset_2017.dta")
casen_2024 <- read_dta("harmonized_data/casen_subset_2024.dta")

cat("2017 observations:", nrow(casen_2017), "\n")
cat("2024 observations:", nrow(casen_2024), "\n")

# ===========================================================================
# CALCULATE HOURLY INCOME BY COHORT
# ===========================================================================

results_2017 <- calculate_hourly_income(casen_2017, 2017, INFLATION_FACTOR_2017_TO_2024)
results_2024 <- calculate_hourly_income(casen_2024, 2024, 1)  # No adjustment needed

# Combine both years
all_results <- bind_rows(results_2017, results_2024)

# ===========================================================================
# CREATE OUTPUT TABLES
# ===========================================================================

# Round all numeric columns
all_results <- all_results %>%
  mutate(across(c(mean, p10, p25, median, p75, p90), ~ round(.x, 0)))

# Create wide tables for each statistic
create_wide_table <- function(data, stat_col) {
  data %>%
    select(year, age_cohort, educ_category, value = all_of(stat_col)) %>%
    pivot_wider(names_from = educ_category, values_from = value) %>%
    select(year, age_cohort, Total, `No University`, University) %>%
    mutate(Univ_Premium_Pct = round(100 * (University / `No University` - 1), 1))
}

wide_mean <- create_wide_table(all_results, "mean")
wide_median <- create_wide_table(all_results, "median")
wide_p10 <- create_wide_table(all_results, "p10")
wide_p25 <- create_wide_table(all_results, "p25")
wide_p75 <- create_wide_table(all_results, "p75")
wide_p90 <- create_wide_table(all_results, "p90")

# ===========================================================================
# COMPARISON TABLES: 2017 vs 2024 changes
# ===========================================================================

create_comparison <- function(wide_data, stat_name) {
  data_2017 <- wide_data %>% filter(year == 2017)
  data_2024 <- wide_data %>% filter(year == 2024)

  data_2017 %>%
    select(age_cohort, Total_2017 = Total, NoUniv_2017 = `No University`, Univ_2017 = University) %>%
    left_join(
      data_2024 %>%
        select(age_cohort, Total_2024 = Total, NoUniv_2024 = `No University`, Univ_2024 = University),
      by = "age_cohort"
    ) %>%
    mutate(
      Total_Change_Pct = round(100 * (Total_2024 / Total_2017 - 1), 1),
      NoUniv_Change_Pct = round(100 * (NoUniv_2024 / NoUniv_2017 - 1), 1),
      Univ_Change_Pct = round(100 * (Univ_2024 / Univ_2017 - 1), 1)
    )
}

comparison_mean <- create_comparison(wide_mean, "mean")
comparison_median <- create_comparison(wide_median, "median")

# ===========================================================================
# SAMPLE SIZES
# ===========================================================================

sample_sizes <- all_results %>%
  select(year, age_cohort, educ_category, n_obs, n_weighted) %>%
  mutate(n_weighted = round(n_weighted, 0))

# ===========================================================================
# SAVE TO EXCEL
# ===========================================================================

cat("\nSaving results to Excel...\n")

wb <- createWorkbook()

# Sheet 1: Mean
addWorksheet(wb, "Mean")
writeData(wb, "Mean", wide_mean)

# Sheet 2: Median
addWorksheet(wb, "Median")
writeData(wb, "Median", wide_median)

# Sheet 3: P10
addWorksheet(wb, "P10")
writeData(wb, "P10", wide_p10)

# Sheet 4: P25
addWorksheet(wb, "P25")
writeData(wb, "P25", wide_p25)

# Sheet 5: P75
addWorksheet(wb, "P75")
writeData(wb, "P75", wide_p75)

# Sheet 6: P90
addWorksheet(wb, "P90")
writeData(wb, "P90", wide_p90)

# Sheet 7: Comparison Mean
addWorksheet(wb, "Comparison_Mean")
writeData(wb, "Comparison_Mean", comparison_mean)

# Sheet 8: Comparison Median
addWorksheet(wb, "Comparison_Median")
writeData(wb, "Comparison_Median", comparison_median)

# Sheet 9: Sample sizes
addWorksheet(wb, "Sample_Sizes")
writeData(wb, "Sample_Sizes", sample_sizes)

# Sheet 10: Long format (all data)
addWorksheet(wb, "All_Data_Long")
writeData(wb, "All_Data_Long", all_results)

saveWorkbook(wb, OUTPUT_FILE, overwrite = TRUE)

cat("Saved to:", OUTPUT_FILE, "\n")

# ===========================================================================
# PRINT SUMMARY
# ===========================================================================

cat("\n========================================\n")
cat("SUMMARY: Hourly Labor Income (CLP 2024)\n")
cat("Employed workers dependent on employer, ages 26-65\n")
cat("========================================\n\n")

cat("MEAN:\n")
print(wide_mean, n = 20)

cat("\nMEDIAN:\n")
print(wide_median, n = 20)

cat("\n========================================\n")
cat("CHANGE 2017 -> 2024 (%) - MEAN\n")
cat("(2017 income adjusted for inflation)\n")
cat("========================================\n\n")

print(comparison_mean %>% select(age_cohort, Total_Change_Pct, NoUniv_Change_Pct, Univ_Change_Pct), n = 10)

cat("\nAnalysis complete.\n")
