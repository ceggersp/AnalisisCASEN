# CASEN - Extended Poverty Rate Comparison
# Total Income vs Autonomous Income by Household Age Composition
# ==================================================
# UNIFIED SCRIPT: Works for any survey year
# ==================================================

library(haven)
library(dplyr)
library(tidyr)
library(writexl)

# ==================================================
# USER CONFIGURATION: SET THE YEAR HERE
# ==================================================

YEAR <- 2024  # Change to 2017 or 2024

# ==================================================
# POVERTY LINES BY YEAR (World Bank 8.3 PPP per day)
# ==================================================

poverty_lines <- list(
  "2017" = 107347,
  "2024" = 152160
)

# Validate year
if (!as.character(YEAR) %in% names(poverty_lines)) {
  stop("Year ", YEAR, " not supported. Available years: ", paste(names(poverty_lines), collapse = ", "))
}

poverty_line <- poverty_lines[[as.character(YEAR)]]

# ==================================================
# PATHS
# ==================================================

setwd("C:/Users/CARLOSEG/Documents/CASEN")

data_file <- paste0("harmonized_data/casen_subset_", YEAR, ".dta")
output_dir <- "output/tables"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ==================================================
# LOAD DATA
# ==================================================

cat("============================================================\n")
cat("CASEN ", YEAR, " - POVERTY ANALYSIS\n", sep = "")
cat("============================================================\n\n")

cat("Loading CASEN", YEAR, "data (subset)...\n")
casen <- read_dta(data_file)

cat("Loaded", nrow(casen), "observations with", ncol(casen), "variables\n")
cat("Poverty line: $", format(poverty_line, big.mark = ","), " per capita (", YEAR, " prices)\n\n", sep = "")

# ==================================================
# CREATE HOUSEHOLD COMPOSITION INDICATORS
# ==================================================
cat("Creating household composition indicators...\n")

# Create household-level indicators
household_indicators <- casen %>%
  group_by(folio) %>%
  summarise(
    # Count members in different age groups
    n_under24 = sum(edad < 24, na.rm = TRUE),
    n_24to64 = sum(edad >= 24 & edad < 65, na.rm = TRUE),
    n_65plus = sum(edad >= 65, na.rm = TRUE),
    total_members = n(),

    # Household composition flags
    has_65plus = n_65plus > 0,
    no_65plus = n_65plus == 0,
    all_65plus = n_65plus == total_members & total_members > 0,
    atleast1_65plus = n_65plus >= 1,
    working_age_only = (n_24to64 == total_members) & (n_under24 == 0) & (n_65plus == 0),

    .groups = "drop"
  )

# Merge back to individual level
casen <- casen %>%
  left_join(household_indicators %>%
              select(folio, no_65plus, atleast1_65plus, only_65plus = all_65plus, working_age_only),
            by = "folio")

# Calculate income variables
casen <- casen %>%
  mutate(
    ypch_total_cor = ytotcorh / numper,
    ypch_true_aut = yautcorh/numper
  )

cat("Household composition indicators created.\n")
cat("  - no_65plus: households with NO members >=65\n")
cat("  - atleast1_65plus: households with AT LEAST ONE member >=65\n")
cat("  - only_65plus: households where ALL members >=65\n")
cat("  - working_age_only: households with only 24-64 year olds\n\n")

# ==================================================
# EXTRACT REGION LABELS
# ==================================================

region_labels <- attr(casen$region, "labels")
if (!is.null(region_labels)) {
  region_df <- data.frame(
    Code = as.numeric(region_labels),
    Region_Name = names(region_labels)
  )
} else {
  region_df <- data.frame(
    Code = unique(casen$region),
    Region_Name = paste("Region", unique(casen$region))
  )
}

# ==================================================
# POVERTY CALCULATION FUNCTIONS
# ==================================================

# Function to calculate weighted poverty rate
calc_poverty_rate <- function(income_vec, weight_vec) {
  valid <- !is.na(income_vec) & !is.na(weight_vec)
  income_vec <- income_vec[valid]
  weight_vec <- weight_vec[valid]

  if (length(income_vec) == 0) return(NA)

  weighted_poor <- sum((income_vec < poverty_line) * weight_vec)
  weighted_total <- sum(weight_vec)

  return((weighted_poor / weighted_total) * 100)
}

# Function to calculate poverty rates for all categories
calc_all_poverty_rates <- function(data) {
  pov_tot <- calc_poverty_rate(data$ypch_total_cor, data$expr)
  pov_aut <- calc_poverty_rate(data$ypch_true_aut, data$expr)

  pov_tot_no65 <- calc_poverty_rate(
    data$ypch_total_cor[data$no_65plus],
    data$expr[data$no_65plus]
  )
  pov_aut_no65 <- calc_poverty_rate(
    data$ypch_true_aut[data$no_65plus],
    data$expr[data$no_65plus]
  )

  pov_tot_atleast1 <- calc_poverty_rate(
    data$ypch_total_cor[data$atleast1_65plus],
    data$expr[data$atleast1_65plus]
  )
  pov_aut_atleast1 <- calc_poverty_rate(
    data$ypch_true_aut[data$atleast1_65plus],
    data$expr[data$atleast1_65plus]
  )

  pov_tot_only65 <- calc_poverty_rate(
    data$ypch_total_cor[data$only_65plus],
    data$expr[data$only_65plus]
  )
  pov_aut_only65 <- calc_poverty_rate(
    data$ypch_true_aut[data$only_65plus],
    data$expr[data$only_65plus]
  )

  pov_tot_workhh <- calc_poverty_rate(
    data$ypch_total_cor[data$working_age_only],
    data$expr[data$working_age_only]
  )
  pov_aut_workhh <- calc_poverty_rate(
    data$ypch_true_aut[data$working_age_only],
    data$expr[data$working_age_only]
  )

  data.frame(
    Pov_Tot = pov_tot,
    Pov_Aut = pov_aut,
    Diff = pov_aut - pov_tot,
    Pov_Tot_No65 = pov_tot_no65,
    Pov_Aut_No65 = pov_aut_no65,
    Diff_No65 = pov_aut_no65 - pov_tot_no65,
    Pov_Tot_AtLeast1_65 = pov_tot_atleast1,
    Pov_Aut_AtLeast1_65 = pov_aut_atleast1,
    Diff_AtLeast1_65 = pov_aut_atleast1 - pov_tot_atleast1,
    Pov_Tot_Only65 = pov_tot_only65,
    Pov_Aut_Only65 = pov_aut_only65,
    Diff_Only65 = pov_aut_only65 - pov_tot_only65,
    Pov_Tot_WorkAge = pov_tot_workhh,
    Pov_Aut_WorkAge = pov_aut_workhh,
    Diff_WorkAge = pov_aut_workhh - pov_tot_workhh
  )
}

# ==================================================
# 1. NATIONAL LEVEL
# ==================================================
cat("Calculating national poverty rates by household composition...\n")

national <- cbind(
  data.frame(Geography = "Chile (National)", Code = NA_real_),
  calc_all_poverty_rates(casen)
)

# ==================================================
# 2. REGIONAL LEVEL
# ==================================================
cat("Calculating regional poverty rates...\n")

regional <- casen %>%
  group_by(region) %>%
  summarise(
    calc_all_poverty_rates(cur_data()),
    .groups = "drop"
  ) %>%
  rename(Code = region) %>%
  left_join(region_df, by = "Code") %>%
  mutate(Geography = Region_Name) %>%
  select(Geography, Code, starts_with("Pov_"), starts_with("Diff")) %>%
  arrange(Code)

# ==================================================
# 3. COMUNA LEVEL - REGION METROPOLITANA ONLY
# ==================================================
cat("Calculating poverty rates for comunas in Region Metropolitana...\n")

rm_data <- casen %>% filter(region == 13)

comuna_labels <- attr(casen$comuna, "labels")
if (!is.null(comuna_labels)) {
  comuna_df <- data.frame(
    Code = as.numeric(comuna_labels),
    Comuna_Name = names(comuna_labels)
  )

  comunal <- rm_data %>%
    group_by(comuna) %>%
    summarise(
      calc_all_poverty_rates(cur_data()),
      .groups = "drop"
    ) %>%
    rename(Code = comuna) %>%
    left_join(comuna_df, by = "Code") %>%
    mutate(Geography = Comuna_Name) %>%
    select(Geography, Code, starts_with("Pov_"), starts_with("Diff")) %>%
    arrange(Code)
} else {
  comunal <- rm_data %>%
    group_by(comuna) %>%
    summarise(
      calc_all_poverty_rates(cur_data()),
      .groups = "drop"
    ) %>%
    rename(Code = comuna) %>%
    mutate(Geography = paste("Comuna", Code)) %>%
    select(Geography, Code, starts_with("Pov_"), starts_with("Diff")) %>%
    arrange(Code)
}

# ==================================================
# 4. COMBINE ALL RESULTS
# ==================================================

poverty_table <- bind_rows(national, regional, comunal)
poverty_table <- poverty_table %>%
  mutate(across(c(starts_with("Pov_"), starts_with("Diff")), ~round(., 2)))

# ==================================================
# 5. HOUSEHOLD TYPE FREQUENCIES
# ==================================================
cat("Calculating household type frequencies...\n")

calc_household_frequencies <- function(data) {
  total_pop <- sum(data$expr, na.rm = TRUE)

  data.frame(
    N_All = total_pop,
    Pct_All = 100,
    N_No65 = sum(data$expr[data$no_65plus], na.rm = TRUE),
    Pct_No65 = 100 * sum(data$expr[data$no_65plus], na.rm = TRUE) / total_pop,
    N_AtLeast1_65 = sum(data$expr[data$atleast1_65plus], na.rm = TRUE),
    Pct_AtLeast1_65 = 100 * sum(data$expr[data$atleast1_65plus], na.rm = TRUE) / total_pop,
    N_Only65 = sum(data$expr[data$only_65plus], na.rm = TRUE),
    Pct_Only65 = 100 * sum(data$expr[data$only_65plus], na.rm = TRUE) / total_pop,
    N_WorkAge = sum(data$expr[data$working_age_only], na.rm = TRUE),
    Pct_WorkAge = 100 * sum(data$expr[data$working_age_only], na.rm = TRUE) / total_pop
  )
}

national_freq <- cbind(
  data.frame(Geography = "Chile (National)", Code = NA_real_),
  calc_household_frequencies(casen)
)

regional_freq <- casen %>%
  group_by(region) %>%
  summarise(calc_household_frequencies(cur_data()), .groups = "drop") %>%
  rename(Code = region) %>%
  left_join(region_df, by = "Code") %>%
  mutate(Geography = Region_Name) %>%
  select(Geography, Code, starts_with("N_"), starts_with("Pct_")) %>%
  arrange(Code)

if (!is.null(attr(casen$comuna, "labels"))) {
  comunal_freq <- rm_data %>%
    group_by(comuna) %>%
    summarise(calc_household_frequencies(cur_data()), .groups = "drop") %>%
    rename(Code = comuna) %>%
    left_join(comuna_df, by = "Code") %>%
    mutate(Geography = Comuna_Name) %>%
    select(Geography, Code, starts_with("N_"), starts_with("Pct_")) %>%
    arrange(Code)
} else {
  comunal_freq <- rm_data %>%
    group_by(comuna) %>%
    summarise(calc_household_frequencies(cur_data()), .groups = "drop") %>%
    rename(Code = comuna) %>%
    mutate(Geography = paste("Comuna", Code)) %>%
    select(Geography, Code, starts_with("N_"), starts_with("Pct_")) %>%
    arrange(Code)
}

percentages_table <- bind_rows(national_freq, regional_freq, comunal_freq)
percentages_table <- percentages_table %>%
  mutate(across(starts_with("N_"), ~round(., 0))) %>%
  mutate(across(starts_with("Pct_"), ~round(., 2)))

# ==================================================
# 6. INCOME DECILE ANALYSIS
# ==================================================
cat("Calculating income deciles...\n")

weighted_quantile <- function(x, weights, probs) {
  valid <- !is.na(x) & !is.na(weights)
  x <- x[valid]
  weights <- weights[valid]

  ord <- order(x)
  x <- x[ord]
  weights <- weights[ord]

  cum_weights <- cumsum(weights)
  total_weight <- sum(weights)
  cum_prop <- cum_weights / total_weight

  sapply(probs, function(p) {
    if (p == 0) return(x[1])
    if (p == 1) return(x[length(x)])
    idx <- which(cum_prop >= p)[1]
    return(x[idx])
  })
}

decile_breaks <- weighted_quantile(
  casen$ypch_true_aut,
  weights = casen$expr,
  probs = seq(0, 1, 0.1)
)

decile_ranges <- data.frame(
  Decile = 1:10,
  Min_Income = c(decile_breaks[1:10]),
  Max_Income = c(decile_breaks[2:11])
)

calc_decile_distribution <- function(data, income_var) {
  data_with_decile <- data %>%
    filter(!is.na(!!sym(income_var)) & !is.na(expr)) %>%
    mutate(
      decile = cut(!!sym(income_var),
                   breaks = c(-Inf, decile_breaks[2:10], Inf),
                   labels = 1:10,
                   include.lowest = TRUE,
                   right = FALSE)
    )

  calc_dist <- function(subset_data) {
    subset_data %>%
      group_by(decile) %>%
      summarise(weight = sum(expr, na.rm = TRUE), .groups = "drop") %>%
      mutate(pct = 100 * weight / sum(weight)) %>%
      pull(pct)
  }

  all_dist <- calc_dist(data_with_decile)
  no65_dist <- calc_dist(data_with_decile %>% filter(no_65plus))
  atleast1_dist <- calc_dist(data_with_decile %>% filter(atleast1_65plus))
  only65_dist <- calc_dist(data_with_decile %>% filter(only_65plus))
  workhh_dist <- calc_dist(data_with_decile %>% filter(working_age_only))

  pad_to_10 <- function(x) {
    if (length(x) == 10) return(x)
    result <- rep(0, 10)
    result[1:length(x)] <- x
    result
  }

  data.frame(
    Decile = 1:10,
    All = pad_to_10(all_dist),
    No65 = pad_to_10(no65_dist),
    AtLeast1_65 = pad_to_10(atleast1_dist),
    Only65 = pad_to_10(only65_dist),
    WorkAge = pad_to_10(workhh_dist)
  )
}

decile_dist_total <- calc_decile_distribution(casen, "ypch_total_cor")
decile_dist_autonomous <- calc_decile_distribution(casen, "ypch_true_aut")

decile_dist_total <- decile_dist_total %>%
  left_join(decile_ranges, by = "Decile") %>%
  select(Decile, Min_Income, Max_Income, All, No65, AtLeast1_65, Only65, WorkAge) %>%
  mutate(across(c(All, No65, AtLeast1_65, Only65, WorkAge), ~round(., 2)))

decile_dist_autonomous <- decile_dist_autonomous %>%
  left_join(decile_ranges, by = "Decile") %>%
  select(Decile, Min_Income, Max_Income, All, No65, AtLeast1_65, Only65, WorkAge) %>%
  mutate(across(c(All, No65, AtLeast1_65, Only65, WorkAge), ~round(., 2)))

# ==================================================
# 7. DOCUMENTATION
# ==================================================

documentation <- data.frame(
  Item = c(
    "Survey Year",
    "Poverty Line",
    "Sample Size",
    "",
    "HOUSEHOLD CATEGORIES",
    "All households",
    "No65",
    "AtLeast1_65",
    "Only65",
    "WorkAge",
    "",
    "INCOME DEFINITIONS",
    "Total income",
    "Autonomous income",
    "",
    "POVERTY MEASURES",
    "Pov_Tot",
    "Pov_Aut",
    "Diff"
  ),
  Description = c(
    paste("CASEN", YEAR),
    paste0("$", format(poverty_line, big.mark = ","), " per capita (monthly, ", YEAR, " prices)"),
    paste(format(nrow(casen), big.mark = ","), "individuals"),
    "",
    "",
    "All households in the geographic unit",
    "Households with NO members aged 65+",
    "Households with AT LEAST ONE member aged 65+",
    "Households where ALL members are aged 65+",
    "Households with members aged 24-64 ONLY (no children, no elderly)",
    "",
    "",
    "ytotcorh / numper: Per capita household income from all sources",
    "(ytotcorh - ysubh) / numper: Excluding state transfers (non-contributory pensions + subsidies)",
    "",
    "",
    "Poverty rate using total income",
    "Poverty rate using autonomous income",
    "Pov_Aut - Pov_Tot = Impact of state transfers (pp)"
  )
)

# ==================================================
# 8. SAVE TO EXCEL
# ==================================================

excel_output <- list(
  Poverty_Rates = poverty_table,
  Percentages = percentages_table,
  Deciles_TotalIncome = decile_dist_total,
  Deciles_AutonomousIncome = decile_dist_autonomous,
  Documentation = documentation
)

output_file_xlsx <- paste0(output_dir, "/poverty_comparison_extended_", YEAR, ".xlsx")
write_xlsx(excel_output, output_file_xlsx)

cat("\n============================================================\n")
cat("OUTPUT SAVED\n")
cat("============================================================\n\n")

cat("Excel file:", output_file_xlsx, "\n")
cat("  - Poverty_Rates (", nrow(poverty_table), " rows)\n")
cat("  - Percentages (", nrow(percentages_table), " rows)\n")
cat("  - Deciles_TotalIncome (10 rows)\n")
cat("  - Deciles_AutonomousIncome (10 rows)\n")
cat("  - Documentation\n\n")

# ==================================================
# 9. SUMMARY OUTPUT
# ==================================================

cat("============================================================\n")
cat("NATIONAL SUMMARY - CASEN ", YEAR, "\n", sep = "")
cat("============================================================\n\n")

cat("All households:\n")
cat("  Total income poverty:", round(national$Pov_Tot, 2), "%\n")
cat("  Autonomous income poverty:", round(national$Pov_Aut, 2), "%\n")
cat("  Impact of state transfers:", round(national$Diff, 2), "pp\n\n")

cat("Households with NO members >=65:\n")
cat("  Total income poverty:", round(national$Pov_Tot_No65, 2), "%\n")
cat("  Autonomous income poverty:", round(national$Pov_Aut_No65, 2), "%\n")
cat("  Impact of state transfers:", round(national$Diff_No65, 2), "pp\n\n")

cat("Households with AT LEAST ONE member >=65:\n")
cat("  Total income poverty:", round(national$Pov_Tot_AtLeast1_65, 2), "%\n")
cat("  Autonomous income poverty:", round(national$Pov_Aut_AtLeast1_65, 2), "%\n")
cat("  Impact of state transfers:", round(national$Diff_AtLeast1_65, 2), "pp\n\n")

cat("Households where ALL members >=65:\n")
cat("  Total income poverty:", round(national$Pov_Tot_Only65, 2), "%\n")
cat("  Autonomous income poverty:", round(national$Pov_Aut_Only65, 2), "%\n")
cat("  Impact of state transfers:", round(national$Diff_Only65, 2), "pp\n\n")

cat("Working-age only households (24-64):\n")
cat("  Total income poverty:", round(national$Pov_Tot_WorkAge, 2), "%\n")
cat("  Autonomous income poverty:", round(national$Pov_Aut_WorkAge, 2), "%\n")
cat("  Impact of state transfers:", round(national$Diff_WorkAge, 2), "pp\n\n")

cat("============================================================\n")
cat("Analysis complete for CASEN ", YEAR, ".\n", sep = "")
cat("============================================================\n")
