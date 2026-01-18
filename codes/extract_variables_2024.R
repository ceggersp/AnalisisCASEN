# CASEN 2024 - Extract Required Variables
# Creates a smaller dataset with only the variables needed for poverty analysis
# ===========================================================================

library(haven)
library(dplyr)

setwd("C:/Users/CARLOSEG/Documents/CASEN")

cat("Loading full CASEN 2024 dataset...\n")
casen_full <- read_dta("raw_data/casen_2024.dta")

cat("Full dataset dimensions: ", nrow(casen_full), " rows x ", ncol(casen_full), " columns\n\n")

# Load provincia/comuna data (separate file in 2024)
cat("Loading provincia/comuna data...\n")
prov_comuna <- read_dta("raw_data/casen_2024_provincia_comuna.dta")

# Merge comuna into main dataset
casen_full <- casen_full %>%
  left_join(prov_comuna %>% select(folio, id_persona, provincia, comuna),
            by = c("folio", "id_persona"))

cat("After merge dimensions: ", nrow(casen_full), " rows x ", ncol(casen_full), " columns\n\n")

# Variables needed for the poverty and labor analysis
vars_needed <- c(
  # Identifiers
  "folio",        # Household ID

  # Geographic
  "region",       # Region code
  "comuna",       # Comuna code

  # Demographics
  "edad",         # Age
  "numper",       # Number of persons in household

  # Income variables (household level)
  "ytotcorh",     # Total household income (corrected)
  "ysubh",        # Household subsidies (state transfers)

  # Labor income variables (individual level)
  "yoprcor",      # Labor income from main occupation (corrected)
  "ytrabajocor",  # Total labor income (corrected)
  "y2_hrs",       # Hours worked (contracted with employer)

  # Occupational status (individual level)
  "activ",        # Activity status: 1=Employed, 2=Unemployed, 3=Inactive
  "o15",          # Employment category: 1=Employer, 2=Self-employed, 3-5=Employee, etc.

  # Education (individual level)
  "e6a",          # Education level: 1=Never, 7=Basic, 9/11=Secondary, 12-15=Tertiary/Postgrad

  # Survey weights
  "expr"          # Expansion factor (survey weight)
)

# Check which variables exist
vars_exist <- vars_needed[vars_needed %in% names(casen_full)]
vars_missing <- vars_needed[!vars_needed %in% names(casen_full)]

if(length(vars_missing) > 0) {
  cat("WARNING: Missing variables:\n")
  print(vars_missing)
  cat("\n")
}

cat("Extracting variables:\n")
print(vars_exist)
cat("\n")

# Extract subset
casen_subset <- casen_full %>%
  select(all_of(vars_exist))

cat("Subset dimensions: ", nrow(casen_subset), " rows x ", ncol(casen_subset), " columns\n\n")

# Preserve variable labels from original dataset
for(var in vars_exist) {
  attr(casen_subset[[var]], "label") <- attr(casen_full[[var]], "label")
  attr(casen_subset[[var]], "labels") <- attr(casen_full[[var]], "labels")
}

# ===========================================================================
# HARMONIZATION: Rename variables to common names across surveys
# ===========================================================================

cat("Harmonizing variable names...\n")

# Rename education variable to common name
if("e6a" %in% names(casen_subset)) {
  casen_subset <- casen_subset %>%
    rename(nivel_educ = e6a)
  cat("  Renamed 'e6a' -> 'nivel_educ'\n")
}

# Create university dummy (harmonized across surveys)
# 2024 e6a codes: 12-15 = Tertiary/Postgrad (Técnico Superior through Doctorado)
casen_subset <- casen_subset %>%
  mutate(
    univ = case_when(
      is.na(nivel_educ) ~ NA_integer_,
      nivel_educ >= 12 & nivel_educ <= 15 ~ 1L,
      TRUE ~ 0L
    )
  )
attr(casen_subset$univ, "label") <- "University education (1=Yes, 0=No)"
cat("  Created 'univ' dummy variable (nivel_educ >= 12)\n")

# Add survey year identifier
casen_subset$year <- 2024L
attr(casen_subset$year, "label") <- "Survey year"
cat("  Added 'year' = 2024\n\n")

# Save as .dta (Stata format) to preserve labels
output_file <- "harmonized_data/casen_subset_2024.dta"
write_dta(casen_subset, output_file)

cat("Saved subset to:", output_file, "\n")

# Also save as RDS for faster R loading
output_file_rds <- "harmonized_data/casen_subset_2024.rds"
saveRDS(casen_subset, output_file_rds)
cat("Saved RDS version to:", output_file_rds, "\n\n")

# Report size reduction
full_size <- file.size("raw_data/casen_2024.dta")
subset_size <- file.size(output_file)
reduction_pct <- round(100 * (1 - subset_size / full_size), 1)

cat("Size comparison:\n")
cat("  Full dataset: ", round(full_size / 1e6, 1), " MB\n")
cat("  Subset: ", round(subset_size / 1e6, 1), " MB\n")
cat("  Reduction: ", reduction_pct, "%\n")

cat("\nExtraction complete.\n")
