# CASEN Comparison Analysis: 2017 vs 2024
# Impact of State Transfers on Poverty Across Years
# ==================================================

library(readxl)
library(haven)
library(dplyr)
library(tidyr)
library(writexl)

setwd("C:/Users/CARLOSEG/Documents/CASEN")

# ==================================================
# POVERTY LINES AND INFLATION ADJUSTMENT
# ==================================================

poverty_line_2017 <- 107347
poverty_line_2024 <- 152160
inflation_factor <- poverty_line_2024 / poverty_line_2017  # ~1.4175

cat("Poverty lines:\n")
cat("  2017: $", format(poverty_line_2017, big.mark = ","), "\n")
cat("  2024: $", format(poverty_line_2024, big.mark = ","), "\n")
cat("  Inflation factor (2017 -> 2024): ", round(inflation_factor, 4), "\n\n")

# ==================================================
# 1. LOAD DATA FROM BOTH EXCEL FILES
# ==================================================

cat("Loading CASEN 2017 Excel data...\n")
poverty_2017 <- read_excel("output/tables/poverty_comparison_extended_2017.xlsx", sheet = "Poverty_Rates")
percentages_2017 <- read_excel("output/tables/poverty_comparison_extended_2017.xlsx", sheet = "Percentages")
deciles_total_2017 <- read_excel("output/tables/poverty_comparison_extended_2017.xlsx", sheet = "Deciles_TotalIncome")
deciles_aut_2017 <- read_excel("output/tables/poverty_comparison_extended_2017.xlsx", sheet = "Deciles_AutonomousIncome")

cat("Loading CASEN 2024 Excel data...\n")
poverty_2024 <- read_excel("output/tables/poverty_comparison_extended_2024.xlsx", sheet = "Poverty_Rates")
percentages_2024 <- read_excel("output/tables/poverty_comparison_extended_2024.xlsx", sheet = "Percentages")
deciles_total_2024 <- read_excel("output/tables/poverty_comparison_extended_2024.xlsx", sheet = "Deciles_TotalIncome")
deciles_aut_2024 <- read_excel("output/tables/poverty_comparison_extended_2024.xlsx", sheet = "Deciles_AutonomousIncome")

cat("Excel data loaded successfully.\n\n")

# ==================================================
# 1B. LOAD RAW SURVEY DATA FOR INCOME CALCULATIONS
# ==================================================
# Using harmonized subset files (run extract_variables_YYYY.R first)

cat("Loading CASEN 2017 subset data...\n")
casen_2017 <- read_dta("harmonized_data/casen_subset_2017.dta")

cat("Loading CASEN 2024 subset data...\n")
casen_2024 <- read_dta("harmonized_data/casen_subset_2024.dta")

# Calculate income variables for both years
casen_2017 <- casen_2017 %>%
  mutate(
    ypch_total = ytotcorh / numper,
    ypch_aut = yautcorh/numper #(ytotcorh - ysubh) / numper
  )

casen_2024 <- casen_2024 %>%
  mutate(
    ypch_total = ytotcorh / numper,
    ypch_aut = yautcorh/numper
  )

cat("Raw survey data loaded and income variables calculated.\n\n")

# ==================================================
# 2. NATIONAL LEVEL COMPARISON
# ==================================================

cat("============================================================\n")
cat("NATIONAL POVERTY COMPARISON: 2017 vs 2024\n")
cat("============================================================\n\n")

# Extract national rows
nat_2017 <- poverty_2017 %>% filter(Geography == "Chile (National)")
nat_2024 <- poverty_2024 %>% filter(Geography == "Chile (National)")

# Create comparison table
national_comparison <- data.frame(
  Metric = c(
    "Overall - Total Income Poverty (%)",
    "Overall - Autonomous Income Poverty (%)",
    "Overall - State Transfer Impact (pp)",
    "",
    "No elderly (No65) - Total Poverty (%)",
    "No elderly (No65) - Autonomous Poverty (%)",
    "No elderly (No65) - Transfer Impact (pp)",
    "",
    "At least 1 elderly (AtLeast1_65) - Total Poverty (%)",
    "At least 1 elderly (AtLeast1_65) - Autonomous Poverty (%)",
    "At least 1 elderly (AtLeast1_65) - Transfer Impact (pp)",
    "",
    "Elderly-only (Only65) - Total Poverty (%)",
    "Elderly-only (Only65) - Autonomous Poverty (%)",
    "Elderly-only (Only65) - Transfer Impact (pp)",
    "",
    "Working-age only (WorkAge) - Total Poverty (%)",
    "Working-age only (WorkAge) - Autonomous Poverty (%)",
    "Working-age only (WorkAge) - Transfer Impact (pp)"
  ),
  CASEN_2017 = c(
    nat_2017$Pov_Tot, nat_2017$Pov_Aut, nat_2017$Diff, NA,
    nat_2017$Pov_Tot_No65, nat_2017$Pov_Aut_No65, nat_2017$Diff_No65, NA,
    nat_2017$Pov_Tot_AtLeast1_65, nat_2017$Pov_Aut_AtLeast1_65, nat_2017$Diff_AtLeast1_65, NA,
    nat_2017$Pov_Tot_Only65, nat_2017$Pov_Aut_Only65, nat_2017$Diff_Only65, NA,
    nat_2017$Pov_Tot_WorkAge, nat_2017$Pov_Aut_WorkAge, nat_2017$Diff_WorkAge
  ),
  CASEN_2024 = c(
    nat_2024$Pov_Tot, nat_2024$Pov_Aut, nat_2024$Diff, NA,
    nat_2024$Pov_Tot_No65, nat_2024$Pov_Aut_No65, nat_2024$Diff_No65, NA,
    nat_2024$Pov_Tot_AtLeast1_65, nat_2024$Pov_Aut_AtLeast1_65, nat_2024$Diff_AtLeast1_65, NA,
    nat_2024$Pov_Tot_Only65, nat_2024$Pov_Aut_Only65, nat_2024$Diff_Only65, NA,
    nat_2024$Pov_Tot_WorkAge, nat_2024$Pov_Aut_WorkAge, nat_2024$Diff_WorkAge
  )
)

national_comparison$Change <- national_comparison$CASEN_2024 - national_comparison$CASEN_2017

print(national_comparison, row.names = FALSE)

# ==================================================
# 3. KEY FINDINGS SUMMARY
# ==================================================

cat("\n\n============================================================\n")
cat("KEY FINDINGS\n")
cat("============================================================\n\n")

# 1. Overall poverty trends
cat("1. OVERALL POVERTY TRENDS\n")
cat("   -----------------------\n")
cat("   Total income poverty: ", nat_2017$Pov_Tot, "% (2017) -> ", nat_2024$Pov_Tot, "% (2024)\n")
cat("   Change: ", round(nat_2024$Pov_Tot - nat_2017$Pov_Tot, 2), " pp\n\n")

cat("   Autonomous income poverty: ", nat_2017$Pov_Aut, "% (2017) -> ", nat_2024$Pov_Aut, "% (2024)\n")
cat("   Change: ", round(nat_2024$Pov_Aut - nat_2017$Pov_Aut, 2), " pp\n\n")

# 2. State transfer effectiveness
cat("2. STATE TRANSFER EFFECTIVENESS\n")
cat("   -----------------------------\n")
cat("   Impact on poverty (overall): ", nat_2017$Diff, " pp (2017) -> ", nat_2024$Diff, " pp (2024)\n")
cat("   Change in effectiveness: ", round(nat_2024$Diff - nat_2017$Diff, 2), " pp\n")
cat("   Interpretation: State transfers became ",
    ifelse(nat_2024$Diff > nat_2017$Diff, "MORE", "LESS"),
    " effective at reducing poverty\n\n")

# 3. Elderly-only households
cat("3. ELDERLY-ONLY HOUSEHOLDS (Only65)\n")
cat("   ---------------------------------\n")
cat("   Total poverty: ", nat_2017$Pov_Tot_Only65, "% (2017) -> ", nat_2024$Pov_Tot_Only65, "% (2024)\n")
cat("   Autonomous poverty: ", nat_2017$Pov_Aut_Only65, "% (2017) -> ", nat_2024$Pov_Aut_Only65, "% (2024)\n")
cat("   Transfer impact: ", nat_2017$Diff_Only65, " pp (2017) -> ", nat_2024$Diff_Only65, " pp (2024)\n")
cat("   Change in impact: ", round(nat_2024$Diff_Only65 - nat_2017$Diff_Only65, 2), " pp\n\n")

# 4. Working-age households
cat("4. WORKING-AGE ONLY HOUSEHOLDS (WorkAge, 24-64)\n")
cat("   ---------------------------------------------\n")
cat("   Total poverty: ", nat_2017$Pov_Tot_WorkAge, "% (2017) -> ", nat_2024$Pov_Tot_WorkAge, "% (2024)\n")
cat("   Autonomous poverty: ", nat_2017$Pov_Aut_WorkAge, "% (2017) -> ", nat_2024$Pov_Aut_WorkAge, "% (2024)\n")
cat("   Transfer impact: ", nat_2017$Diff_WorkAge, " pp (2017) -> ", nat_2024$Diff_WorkAge, " pp (2024)\n\n")

# ==================================================
# 4. REGIONAL COMPARISON
# ==================================================

cat("\n============================================================\n")
cat("REGIONAL COMPARISON: 2017 vs 2024\n")
cat("============================================================\n\n")

# Get regional data (excluding national and comunas)
regions_2017 <- poverty_2017 %>%
  filter(!is.na(Code) & Code < 100) %>%
  select(Geography, Code, Pov_Tot, Pov_Aut, Diff, Diff_Only65)

regions_2024 <- poverty_2024 %>%
  filter(!is.na(Code) & Code < 100) %>%
  select(Geography, Code, Pov_Tot, Pov_Aut, Diff, Diff_Only65)

# Merge by region code
regional_comparison <- regions_2017 %>%
  inner_join(regions_2024, by = "Code", suffix = c("_2017", "_2024")) %>%
  mutate(
    Pov_Tot_Change = Pov_Tot_2024 - Pov_Tot_2017,
    Pov_Aut_Change = Pov_Aut_2024 - Pov_Aut_2017,
    Diff_Change = Diff_2024 - Diff_2017,
    Diff_Only65_Change = Diff_Only65_2024 - Diff_Only65_2017
  ) %>%
  select(Geography_2024, Code,
         Pov_Tot_2017, Pov_Tot_2024, Pov_Tot_Change,
         Diff_2017, Diff_2024, Diff_Change) %>%
  arrange(Code)

colnames(regional_comparison) <- c("Region", "Code",
                                    "TotPov_2017", "TotPov_2024", "TotPov_Chg",
                                    "Impact_2017", "Impact_2024", "Impact_Chg")

print(as.data.frame(regional_comparison), row.names = FALSE)

# ==================================================
# 5. REGIONS WITH LARGEST CHANGES
# ==================================================

cat("\n\n============================================================\n")
cat("REGIONS WITH NOTABLE CHANGES\n")
cat("============================================================\n\n")

# Largest increase in total poverty
cat("Largest INCREASE in total income poverty:\n")
top_increase <- regional_comparison %>%
  arrange(desc(TotPov_Chg)) %>%
  head(3)
for(i in 1:3) {
  cat("  ", i, ". ", top_increase$Region[i], ": +", round(top_increase$TotPov_Chg[i], 2), " pp\n", sep = "")
}

cat("\nLargest DECREASE in total income poverty:\n")
top_decrease <- regional_comparison %>%
  arrange(TotPov_Chg) %>%
  head(3)
for(i in 1:3) {
  cat("  ", i, ". ", top_decrease$Region[i], ": ", round(top_decrease$TotPov_Chg[i], 2), " pp\n", sep = "")
}

cat("\nLargest INCREASE in transfer effectiveness:\n")
top_impact_increase <- regional_comparison %>%
  arrange(desc(Impact_Chg)) %>%
  head(3)
for(i in 1:3) {
  cat("  ", i, ". ", top_impact_increase$Region[i], ": +", round(top_impact_increase$Impact_Chg[i], 2), " pp\n", sep = "")
}

# ==================================================
# 6. DEMOGRAPHIC SHIFTS
# ==================================================

cat("\n\n============================================================\n")
cat("DEMOGRAPHIC SHIFTS: HOUSEHOLD COMPOSITION\n")
cat("============================================================\n\n")

# Extract national percentages
pct_nat_2017 <- percentages_2017 %>% filter(Geography == "Chile (National)")
pct_nat_2024 <- percentages_2024 %>% filter(Geography == "Chile (National)")

demo_comparison <- data.frame(
  Category = c("No elderly (No65)",
               "At least 1 elderly (AtLeast1_65)",
               "Elderly-only (Only65)",
               "Working-age only (WorkAge)"),
  Pct_2017 = c(pct_nat_2017$Pct_No65,
               pct_nat_2017$Pct_AtLeast1_65,
               pct_nat_2017$Pct_Only65,
               pct_nat_2017$Pct_WorkAge),
  Pct_2024 = c(pct_nat_2024$Pct_No65,
               pct_nat_2024$Pct_AtLeast1_65,
               pct_nat_2024$Pct_Only65,
               pct_nat_2024$Pct_WorkAge)
)
demo_comparison$Change <- round(demo_comparison$Pct_2024 - demo_comparison$Pct_2017, 2)

print(demo_comparison, row.names = FALSE)

cat("\nInterpretation:\n")
if(demo_comparison$Change[3] > 0) {
  cat("  - Elderly-only households increased by ", demo_comparison$Change[3], " pp\n", sep = "")
  cat("  - This demographic shift means more people depend on transfers\n")
}

# ==================================================
# 7. DECILE ANALYSIS COMPARISON
# ==================================================

cat("\n\n============================================================\n")
cat("INCOME DISTRIBUTION: DECILE ANALYSIS\n")
cat("============================================================\n\n")

cat("Elderly-only households - Distribution by decile (using TOTAL income boundaries):\n\n")

decile_elderly_comparison <- data.frame(
  Decile = 1:10,
  Only65_Total_2017 = deciles_total_2017$Only65,
  Only65_Total_2024 = deciles_total_2024$Only65,
  Only65_Aut_2017 = deciles_aut_2017$Only65,
  Only65_Aut_2024 = deciles_aut_2024$Only65
)

decile_elderly_comparison <- decile_elderly_comparison %>%
  mutate(
    Total_Change = Only65_Total_2024 - Only65_Total_2017,
    Aut_Change = Only65_Aut_2024 - Only65_Aut_2017
  )

print(decile_elderly_comparison, row.names = FALSE)

cat("\nKey observation:\n")
# Bottom 3 deciles concentration
bottom3_total_2017 <- sum(deciles_total_2017$Only65[1:3])
bottom3_total_2024 <- sum(deciles_total_2024$Only65[1:3])
bottom3_aut_2017 <- sum(deciles_aut_2017$Only65[1:3])
bottom3_aut_2024 <- sum(deciles_aut_2024$Only65[1:3])

cat("  Elderly-only in bottom 3 deciles (TOTAL income):\n")
cat("    2017: ", round(bottom3_total_2017, 1), "%  ->  2024: ", round(bottom3_total_2024, 1), "%\n", sep = "")
cat("  Elderly-only in bottom 3 deciles (AUTONOMOUS income):\n")
cat("    2017: ", round(bottom3_aut_2017, 1), "%  ->  2024: ", round(bottom3_aut_2024, 1), "%\n", sep = "")

# ==================================================
# 8. TRANSFER EFFICIENCY ANALYSIS
# ==================================================

cat("\n\n============================================================\n")
cat("TRANSFER EFFICIENCY ANALYSIS\n")
cat("============================================================\n\n")

# Calculate relative reduction (% of autonomous poverty eliminated)
relative_reduction_2017 <- (nat_2017$Diff / nat_2017$Pov_Aut) * 100
relative_reduction_2024 <- (nat_2024$Diff / nat_2024$Pov_Aut) * 100

cat("Relative poverty reduction (% of autonomous poverty eliminated by transfers):\n")
cat("  Overall: ", round(relative_reduction_2017, 1), "% (2017) -> ", round(relative_reduction_2024, 1), "% (2024)\n\n", sep = "")

# By household type
efficiency_table <- data.frame(
  Category = c("Overall", "No elderly", "At least 1 elderly", "Elderly-only", "Working-age only"),
  Aut_Pov_2017 = c(nat_2017$Pov_Aut, nat_2017$Pov_Aut_No65, nat_2017$Pov_Aut_AtLeast1_65,
                   nat_2017$Pov_Aut_Only65, nat_2017$Pov_Aut_WorkAge),
  Impact_2017 = c(nat_2017$Diff, nat_2017$Diff_No65, nat_2017$Diff_AtLeast1_65,
                  nat_2017$Diff_Only65, nat_2017$Diff_WorkAge),
  Aut_Pov_2024 = c(nat_2024$Pov_Aut, nat_2024$Pov_Aut_No65, nat_2024$Pov_Aut_AtLeast1_65,
                   nat_2024$Pov_Aut_Only65, nat_2024$Pov_Aut_WorkAge),
  Impact_2024 = c(nat_2024$Diff, nat_2024$Diff_No65, nat_2024$Diff_AtLeast1_65,
                  nat_2024$Diff_Only65, nat_2024$Diff_WorkAge)
)

efficiency_table <- efficiency_table %>%
  mutate(
    Efficiency_2017 = round(100 * Impact_2017 / Aut_Pov_2017, 1),
    Efficiency_2024 = round(100 * Impact_2024 / Aut_Pov_2024, 1),
    Efficiency_Change = Efficiency_2024 - Efficiency_2017
  )

cat("Transfer efficiency by household type (% of autonomous poverty eliminated):\n\n")
print(efficiency_table %>% select(Category, Efficiency_2017, Efficiency_2024, Efficiency_Change), row.names = FALSE)

# ==================================================
# 9. AVERAGE INCOME BY DECILE (2017 vs 2024)
# ==================================================

cat("\n\n============================================================\n")
cat("AVERAGE INCOME BY DECILE (2017 in 2024 prices)\n")
cat("============================================================\n\n")

# Function to calculate weighted mean
weighted_mean <- function(x, w) {
  valid <- !is.na(x) & !is.na(w)
  sum(x[valid] * w[valid]) / sum(w[valid])
}

# Function to assign deciles based on total income
assign_deciles <- function(data, income_var, weight_var) {
  # Calculate weighted quantiles for decile boundaries
  valid <- !is.na(data[[income_var]]) & !is.na(data[[weight_var]])
  x <- data[[income_var]][valid]
  w <- data[[weight_var]][valid]

  # Sort by income

  ord <- order(x)
  x_sorted <- x[ord]
  w_sorted <- w[ord]

  # Cumulative weights
  cum_w <- cumsum(w_sorted)
  total_w <- sum(w_sorted)
  cum_prop <- cum_w / total_w

  # Find decile boundaries
  breaks <- sapply(seq(0.1, 0.9, 0.1), function(p) {
    idx <- which(cum_prop >= p)[1]
    x_sorted[idx]
  })
  breaks <- c(-Inf, breaks, Inf)

  # Assign deciles to all observations
  data$decile <- cut(data[[income_var]], breaks = breaks, labels = 1:10, include.lowest = TRUE)
  return(data)
}

# Assign deciles based on AUTONOMOUS income for each year
casen_2017 <- assign_deciles(casen_2017, "ypch_aut", "expr")
casen_2024 <- assign_deciles(casen_2024, "ypch_aut", "expr")

# Calculate average incomes by decile for 2017
decile_income_2017 <- casen_2017 %>%
  filter(!is.na(decile)) %>%
  group_by(decile) %>%
  summarise(
    Avg_Total_2017 = weighted_mean(ypch_total, expr),
    Avg_Aut_2017 = weighted_mean(ypch_aut, expr),
    Avg_Sub_2017 = weighted_mean(ypch_total - ypch_aut, expr),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert to 2024 prices
    Avg_Total_2017_adj = Avg_Total_2017 * inflation_factor,
    Avg_Aut_2017_adj = Avg_Aut_2017 * inflation_factor,
    Avg_Sub_2017_adj = Avg_Sub_2017 * inflation_factor
  )

# Calculate average incomes by decile for 2024
decile_income_2024 <- casen_2024 %>%
  filter(!is.na(decile)) %>%
  group_by(decile) %>%
  summarise(
    Avg_Total_2024 = weighted_mean(ypch_total, expr),
    Avg_Aut_2024 = weighted_mean(ypch_aut, expr),
    Avg_Sub_2024 = weighted_mean(ypch_total - ypch_aut, expr),
    .groups = "drop"
  )

# Merge into comparison table
decile_avg_income <- decile_income_2017 %>%
  left_join(decile_income_2024, by = "decile") %>%
  mutate(
    Decile = as.numeric(as.character(decile)),
    # All values in 2024 prices
    Total_2017 = round(Avg_Total_2017_adj, 0),
    Total_2024 = round(Avg_Total_2024, 0),
    Total_Change = round(Avg_Total_2024 - Avg_Total_2017_adj, 0),
    Total_Change_Pct = round(100 * (Avg_Total_2024 - Avg_Total_2017_adj) / Avg_Total_2017_adj, 1),
    Aut_2017 = round(Avg_Aut_2017_adj, 0),
    Aut_2024 = round(Avg_Aut_2024, 0),
    Aut_Change = round(Avg_Aut_2024 - Avg_Aut_2017_adj, 0),
    Aut_Change_Pct = round(100 * (Avg_Aut_2024 - Avg_Aut_2017_adj) / Avg_Aut_2017_adj, 1),
    Sub_2017 = round(Avg_Sub_2017_adj, 0),
    Sub_2024 = round(Avg_Sub_2024, 0),
    Sub_Change = round(Avg_Sub_2024 - Avg_Sub_2017_adj, 0),
    Sub_Change_Pct = round(100 * (Avg_Sub_2024 - Avg_Sub_2017_adj) / Avg_Sub_2017_adj, 1)
  ) %>%
  select(Decile, Total_2017, Total_2024, Total_Change, Total_Change_Pct,
         Aut_2017, Aut_2024, Aut_Change, Aut_Change_Pct,
         Sub_2017, Sub_2024, Sub_Change, Sub_Change_Pct) %>%
  arrange(Decile)

cat("Average per capita income by decile (2024 prices, CLP):\n\n")
print(as.data.frame(decile_avg_income), row.names = FALSE)

# Summary statistics
cat("\n\nSummary:\n")
cat("  Total income growth (median decile 5): ", decile_avg_income$Total_Change_Pct[5], "%\n", sep = "")
cat("  Autonomous income growth (median decile 5): ", decile_avg_income$Aut_Change_Pct[5], "%\n", sep = "")
cat("\n  Decile 1 total income growth: ", decile_avg_income$Total_Change_Pct[1], "%\n", sep = "")
cat("  Decile 10 total income growth: ", decile_avg_income$Total_Change_Pct[10], "%\n", sep = "")

# ==================================================
# 10. SAVE COMPARISON TABLES TO EXCEL
# ==================================================

cat("\n\n============================================================\n")
cat("SAVING COMPARISON TABLES\n")
cat("============================================================\n\n")

# Create a comprehensive comparison workbook
comparison_output <- list(
  National_Comparison = national_comparison,
  Regional_Comparison = regional_comparison,
  Demographic_Shifts = demo_comparison,
  Decile_Elderly = decile_elderly_comparison,
  Transfer_Efficiency = efficiency_table,
  Decile_Avg_Income = decile_avg_income
)

output_file <- "output/tables/comparison_2017_2024.xlsx"
write_xlsx(comparison_output, output_file)
cat("Comparison tables saved to:", output_file, "\n")

# ==================================================
# 11. SUMMARY STATISTICS FOR REPORT
# ==================================================

cat("\n\n============================================================\n")
cat("SUMMARY STATISTICS FOR MARKDOWN REPORT\n")
cat("============================================================\n\n")

summary_stats <- list(
  # Overall poverty
  total_pov_2017 = nat_2017$Pov_Tot,
  total_pov_2024 = nat_2024$Pov_Tot,
  total_pov_change = round(nat_2024$Pov_Tot - nat_2017$Pov_Tot, 2),

  # Autonomous poverty
  aut_pov_2017 = nat_2017$Pov_Aut,
  aut_pov_2024 = nat_2024$Pov_Aut,
  aut_pov_change = round(nat_2024$Pov_Aut - nat_2017$Pov_Aut, 2),

  # Transfer impact
  impact_2017 = nat_2017$Diff,
  impact_2024 = nat_2024$Diff,
  impact_change = round(nat_2024$Diff - nat_2017$Diff, 2),

  # Elderly-only
  elderly_total_2017 = nat_2017$Pov_Tot_Only65,
  elderly_total_2024 = nat_2024$Pov_Tot_Only65,
  elderly_aut_2017 = nat_2017$Pov_Aut_Only65,
  elderly_aut_2024 = nat_2024$Pov_Aut_Only65,
  elderly_impact_2017 = nat_2017$Diff_Only65,
  elderly_impact_2024 = nat_2024$Diff_Only65,

  # Working-age
  workage_total_2017 = nat_2017$Pov_Tot_WorkAge,
  workage_total_2024 = nat_2024$Pov_Tot_WorkAge,
  workage_impact_2017 = nat_2017$Diff_WorkAge,
  workage_impact_2024 = nat_2024$Diff_WorkAge,

  # Demographics
  elderly_only_pct_2017 = pct_nat_2017$Pct_Only65,
  elderly_only_pct_2024 = pct_nat_2024$Pct_Only65,

  # Efficiency
  efficiency_overall_2017 = round(relative_reduction_2017, 1),
  efficiency_overall_2024 = round(relative_reduction_2024, 1),

  # Regional extremes
  region_max_increase = top_increase$Region[1],
  region_max_increase_pp = round(top_increase$TotPov_Chg[1], 2),
  region_max_decrease = top_decrease$Region[1],
  region_max_decrease_pp = round(top_decrease$TotPov_Chg[1], 2)
)

# Print summary
cat("Total poverty: ", summary_stats$total_pov_2017, "% (2017) -> ", summary_stats$total_pov_2024, "% (2024)\n")
cat("Autonomous poverty: ", summary_stats$aut_pov_2017, "% (2017) -> ", summary_stats$aut_pov_2024, "% (2024)\n")
cat("Transfer impact: ", summary_stats$impact_2017, " pp (2017) -> ", summary_stats$impact_2024, " pp (2024)\n")
cat("Efficiency: ", summary_stats$efficiency_overall_2017, "% (2017) -> ", summary_stats$efficiency_overall_2024, "% (2024)\n")

cat("\nAnalysis complete.\n")
