# Generate tables and graphs for Labor Income by Cohort analysis
# For inclusion in the LaTeX report
# ===========================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

setwd("C:/Users/CARLOSEG/Documents/CASEN")

# ===========================================================================
# LOAD DATA
# ===========================================================================

cat("Loading labor income data...\n")

# Read all sheets
mean_data <- read_excel("output/tables/labor_income_by_cohort.xlsx", sheet = "Mean")
median_data <- read_excel("output/tables/labor_income_by_cohort.xlsx", sheet = "Median")
p25_data <- read_excel("output/tables/labor_income_by_cohort.xlsx", sheet = "P25")
p75_data <- read_excel("output/tables/labor_income_by_cohort.xlsx", sheet = "P75")
p90_data <- read_excel("output/tables/labor_income_by_cohort.xlsx", sheet = "P90")

# ===========================================================================
# TABLE 1: HOURLY INCOME BY COHORT AND EDUCATION
# ===========================================================================

cat("\n=== TABLE 1: Hourly Labor Income (CLP/hour, 2024 prices) ===\n\n")

# Function to create wide table for a specific percentile
create_income_table <- function(data, stat_name) {
  # Separate by year
  d2017 <- data %>% filter(year == 2017) %>%
    select(age_cohort, Total_2017 = Total, NoUniv_2017 = `No University`, Univ_2017 = University)
  d2024 <- data %>% filter(year == 2024) %>%
    select(age_cohort, Total_2024 = Total, NoUniv_2024 = `No University`, Univ_2024 = University)

  # Join
  combined <- d2017 %>%
    left_join(d2024, by = "age_cohort")

  # Add totals (weighted average across cohorts - approximate)
  totals_2017 <- data %>% filter(year == 2017) %>%
    summarise(Total = mean(Total), `No University` = mean(`No University`), University = mean(University))
  totals_2024 <- data %>% filter(year == 2024) %>%
    summarise(Total = mean(Total), `No University` = mean(`No University`), University = mean(University))

  total_row <- tibble(
    age_cohort = "Total (avg)",
    Total_2017 = round(totals_2017$Total),
    NoUniv_2017 = round(totals_2017$`No University`),
    Univ_2017 = round(totals_2017$University),
    Total_2024 = round(totals_2024$Total),
    NoUniv_2024 = round(totals_2024$`No University`),
    Univ_2024 = round(totals_2024$University)
  )

  combined <- bind_rows(combined, total_row)
  combined
}

table1_p25 <- create_income_table(p25_data, "P25")
table1_median <- create_income_table(median_data, "Median")
table1_p75 <- create_income_table(p75_data, "P75")
table1_p90 <- create_income_table(p90_data, "P90")

cat("P25:\n")
print(table1_p25)
cat("\nMedian:\n")
print(table1_median)
cat("\nP75:\n")
print(table1_p75)
cat("\nP90:\n")
print(table1_p90)

# ===========================================================================
# TABLE 2: EDUCATION PREMIUM BY COHORT
# ===========================================================================

cat("\n\n=== TABLE 2: University Education Premium (%) ===\n\n")

create_premium_table <- function(data, stat_name) {
  data %>%
    select(year, age_cohort, Univ_Premium_Pct) %>%
    pivot_wider(names_from = year, values_from = Univ_Premium_Pct, names_prefix = "Premium_") %>%
    mutate(Change = Premium_2024 - Premium_2017)
}

premium_p25 <- create_premium_table(p25_data, "P25")
premium_median <- create_premium_table(median_data, "Median")
premium_p75 <- create_premium_table(p75_data, "P75")
premium_p90 <- create_premium_table(p90_data, "P90")

# Add average row
add_avg_row <- function(df) {
  avg_row <- tibble(
    age_cohort = "Average",
    Premium_2017 = round(mean(df$Premium_2017, na.rm = TRUE), 1),
    Premium_2024 = round(mean(df$Premium_2024, na.rm = TRUE), 1),
    Change = round(mean(df$Change, na.rm = TRUE), 1)
  )
  bind_rows(df, avg_row)
}

premium_p25 <- add_avg_row(premium_p25)
premium_median <- add_avg_row(premium_median)
premium_p75 <- add_avg_row(premium_p75)
premium_p90 <- add_avg_row(premium_p90)

cat("P25 Premium:\n")
print(premium_p25)
cat("\nMedian Premium:\n")
print(premium_median)
cat("\nP75 Premium:\n")
print(premium_p75)
cat("\nP90 Premium:\n")
print(premium_p90)

# ===========================================================================
# BAR GRAPHS: EDUCATION PREMIUM BY COHORT
# ===========================================================================

cat("\n\nGenerating bar graphs...\n")

# Prepare data for plotting
prepare_plot_data <- function(data, stat_name) {
  data %>%
    filter(age_cohort != "Average") %>%
    select(age_cohort, Premium_2017, Premium_2024) %>%
    pivot_longer(cols = c(Premium_2017, Premium_2024),
                 names_to = "Year", values_to = "Premium") %>%
    mutate(Year = gsub("Premium_", "", Year))
}

plot_p25 <- prepare_plot_data(premium_p25, "P25")
plot_median <- prepare_plot_data(premium_median, "Median")
plot_p75 <- prepare_plot_data(premium_p75, "P75")
plot_p90 <- prepare_plot_data(premium_p90, "P90")

# Create bar plot function (bilingual: English and Spanish)
create_premium_plot <- function(data, title, subtitle, x_label, y_label, legend_title, filename) {
  p <- ggplot(data, aes(x = age_cohort, y = Premium, fill = Year)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = paste0(round(Premium), "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 5) +
    scale_fill_manual(values = c("2017" = "#6B7B8C", "2024" = "#A3B1BF")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      fill = legend_title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14, color = "gray40"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(data$Premium) * 1.15)

  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
  p
}

# ===========================================================================
# ENGLISH PLOTS
# ===========================================================================

cat("\nGenerating English plots...\n")

p1_en <- create_premium_plot(plot_p25, "University Education Premium (P25)",
                              "University vs No University hourly income",
                              "Age Cohort", "Education Premium (%)", "Year",
                              "output/figures/education_premium_p25.png")
p2_en <- create_premium_plot(plot_median, "University Education Premium (Median)",
                              "University vs No University hourly income",
                              "Age Cohort", "Education Premium (%)", "Year",
                              "output/figures/education_premium_median.png")
p3_en <- create_premium_plot(plot_p75, "University Education Premium (P75)",
                              "University vs No University hourly income",
                              "Age Cohort", "Education Premium (%)", "Year",
                              "output/figures/education_premium_p75.png")
p4_en <- create_premium_plot(plot_p90, "University Education Premium (P90)",
                              "University vs No University hourly income",
                              "Age Cohort", "Education Premium (%)", "Year",
                              "output/figures/education_premium_p90.png")

# ===========================================================================
# SPANISH PLOTS
# ===========================================================================

cat("\nGenerating Spanish plots...\n")

# Prepare Spanish data (translate Year labels)
prepare_plot_data_es <- function(data, stat_name) {
  data %>%
    filter(age_cohort != "Average") %>%
    select(age_cohort, Premium_2017, Premium_2024) %>%
    pivot_longer(cols = c(Premium_2017, Premium_2024),
                 names_to = "Año", values_to = "Premium") %>%
    mutate(Año = gsub("Premium_", "", Año))
}

plot_p25_es <- prepare_plot_data_es(premium_p25, "P25")
plot_median_es <- prepare_plot_data_es(premium_median, "Median")
plot_p75_es <- prepare_plot_data_es(premium_p75, "P75")
plot_p90_es <- prepare_plot_data_es(premium_p90, "P90")

# Spanish plot function
create_premium_plot_es <- function(data, title, subtitle, x_label, y_label, legend_title, filename) {
  p <- ggplot(data, aes(x = age_cohort, y = Premium, fill = Año)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = paste0(round(Premium), "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 5) +
    scale_fill_manual(values = c("2017" = "#6B7B8C", "2024" = "#A3B1BF")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_label,
      y = y_label,
      fill = legend_title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14, color = "gray40"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(data$Premium) * 1.15)

  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
  p
}

p1_es <- create_premium_plot_es(plot_p25_es, "Prima Educacional Universitaria (P25)",
                                 "Ingreso por hora: Universitarios vs No Universitarios",
                                 "Cohorte de Edad", "Prima Educacional (%)", "Año",
                                 "output/figures/education_premium_p25_es.png")
p2_es <- create_premium_plot_es(plot_median_es, "Prima Educacional Universitaria (Mediana)",
                                 "Ingreso por hora: Universitarios vs No Universitarios",
                                 "Cohorte de Edad", "Prima Educacional (%)", "Año",
                                 "output/figures/education_premium_median_es.png")
p3_es <- create_premium_plot_es(plot_p75_es, "Prima Educacional Universitaria (P75)",
                                 "Ingreso por hora: Universitarios vs No Universitarios",
                                 "Cohorte de Edad", "Prima Educacional (%)", "Año",
                                 "output/figures/education_premium_p75_es.png")
p4_es <- create_premium_plot_es(plot_p90_es, "Prima Educacional Universitaria (P90)",
                                 "Ingreso por hora: Universitarios vs No Universitarios",
                                 "Cohorte de Edad", "Prima Educacional (%)", "Año",
                                 "output/figures/education_premium_p90_es.png")

# ===========================================================================
# SAVE TABLES TO EXCEL
# ===========================================================================

cat("\nSaving tables to Excel...\n")

wb <- createWorkbook()

addWorksheet(wb, "Income_P25")
writeData(wb, "Income_P25", table1_p25)

addWorksheet(wb, "Income_Median")
writeData(wb, "Income_Median", table1_median)

addWorksheet(wb, "Income_P75")
writeData(wb, "Income_P75", table1_p75)

addWorksheet(wb, "Income_P90")
writeData(wb, "Income_P90", table1_p90)

addWorksheet(wb, "Premium_P25")
writeData(wb, "Premium_P25", premium_p25)

addWorksheet(wb, "Premium_Median")
writeData(wb, "Premium_Median", premium_median)

addWorksheet(wb, "Premium_P75")
writeData(wb, "Premium_P75", premium_p75)

addWorksheet(wb, "Premium_P90")
writeData(wb, "Premium_P90", premium_p90)

saveWorkbook(wb, "output/tables/labor_income_tables.xlsx", overwrite = TRUE)
cat("Saved: output/tables/labor_income_tables.xlsx\n")

# ===========================================================================
# PRINT LATEX TABLES
# ===========================================================================

cat("\n\n========================================\n")
cat("LATEX TABLE CODE\n")
cat("========================================\n\n")

# Function to generate LaTeX table for income
generate_latex_income_table <- function(df, caption, label) {
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\small\n")
  cat("\\begin{tabular}{@{}lrrrrrr@{}}\n")
  cat("\\toprule\n")
  cat("& \\multicolumn{3}{c}{\\textbf{2017}} & \\multicolumn{3}{c}{\\textbf{2024}} \\\\\n")
  cat("\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n")
  cat("\\textbf{Cohort} & Total & No Univ & Univ & Total & No Univ & Univ \\\\\n")
  cat("\\midrule\n")
  for (i in 1:nrow(df)) {
    row <- df[i,]
    if (row$age_cohort == "Total (avg)") cat("\\midrule\n")
    cat(sprintf("%s & %s & %s & %s & %s & %s & %s \\\\\n",
                row$age_cohort,
                format(row$Total_2017, big.mark = ","),
                format(row$NoUniv_2017, big.mark = ","),
                format(row$Univ_2017, big.mark = ","),
                format(row$Total_2024, big.mark = ","),
                format(row$NoUniv_2024, big.mark = ","),
                format(row$Univ_2024, big.mark = ",")))
  }
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  cat("\\end{table}\n\n")
}

# Function to generate LaTeX table for premium
generate_latex_premium_table <- function(df, caption, label) {
  cat("\\begin{table}[H]\n")
  cat("\\centering\n")
  cat("\\begin{tabular}{@{}lrrr@{}}\n")
  cat("\\toprule\n")
  cat("\\textbf{Cohort} & \\textbf{2017} & \\textbf{2024} & \\textbf{Change} \\\\\n")
  cat("\\midrule\n")
  for (i in 1:nrow(df)) {
    row <- df[i,]
    if (row$age_cohort == "Average") cat("\\midrule\n")
    cat(sprintf("%s & %.1f\\%% & %.1f\\%% & %.1f pp \\\\\n",
                row$age_cohort,
                row$Premium_2017,
                row$Premium_2024,
                row$Change))
  }
  cat("\\bottomrule\n")
  cat("\\end{tabular}\n")
  cat(sprintf("\\caption{%s}\n", caption))
  cat(sprintf("\\label{%s}\n", label))
  cat("\\end{table}\n\n")
}

cat("% Income Tables (P25, Median, P75, P90)\n\n")
generate_latex_income_table(table1_median, "Median hourly labor income by age cohort and education (CLP/hour, 2024 prices)", "tab:income_median")

cat("\n% Premium Tables\n\n")
generate_latex_premium_table(premium_median, "University education premium on median hourly income (\\%)", "tab:premium_median")

# ===========================================================================
# NEW TABLE: INCOME BY PERCENTILE WITH PREMIA (Table 17)
# ===========================================================================

cat("\n\n========================================\n")
cat("TABLE 17: Income by Percentile with Premia\n")
cat("========================================\n\n")

# Create summary table across percentiles (average across cohorts)
create_percentile_summary <- function() {
  # Get average income across cohorts for each percentile
  get_avg <- function(data, pct_name) {
    d2017 <- data %>% filter(year == 2017) %>%
      summarise(NoUniv = mean(`No University`), Univ = mean(University))
    d2024 <- data %>% filter(year == 2024) %>%
      summarise(NoUniv = mean(`No University`), Univ = mean(University))

    tibble(
      Percentile = pct_name,
      NoUniv_2017 = round(d2017$NoUniv),
      Univ_2017 = round(d2017$Univ),
      Premium_2017 = round(100 * (d2017$Univ / d2017$NoUniv - 1), 1),
      NoUniv_2024 = round(d2024$NoUniv),
      Univ_2024 = round(d2024$Univ),
      Premium_2024 = round(100 * (d2024$Univ / d2024$NoUniv - 1), 1)
    )
  }

  bind_rows(
    get_avg(p25_data, "P25"),
    get_avg(median_data, "Median"),
    get_avg(p75_data, "P75"),
    get_avg(p90_data, "P90")
  ) %>%
    mutate(Premium_Change = Premium_2024 - Premium_2017)
}

table17 <- create_percentile_summary()
print(table17)

# Generate LaTeX for Table 17
cat("\n% LaTeX for Table 17\n\n")
cat("\\begin{table}[H]\n")
cat("\\centering\n")
cat("\\caption{Average education premium by percentile (all cohorts)}\n")
cat("\\label{tab:premium_summary}\n")
cat("\\begin{tabular}{@{}lrrrrrrrr@{}}\n")
cat("\\toprule\n")
cat("& \\multicolumn{3}{c}{\\textbf{2017 (adj.)}} & \\multicolumn{3}{c}{\\textbf{2024}} & \\\\\n")
cat("\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}\n")
cat("\\textbf{Percentile} & No Univ & Univ & Premium & No Univ & Univ & Premium & \\textbf{Change} \\\\\n")
cat("\\midrule\n")
for (i in 1:nrow(table17)) {
  row <- table17[i,]
  cat(sprintf("%s & %s & %s & %.1f\\%% & %s & %s & %.1f\\%% & %.1f pp \\\\\n",
              row$Percentile,
              format(row$NoUniv_2017, big.mark = ","),
              format(row$Univ_2017, big.mark = ","),
              row$Premium_2017,
              format(row$NoUniv_2024, big.mark = ","),
              format(row$Univ_2024, big.mark = ","),
              row$Premium_2024,
              row$Premium_Change))
}
cat("\\bottomrule\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n\n")

# ===========================================================================
# NEW BAR GRAPHS: Non-Univ vs Univ Income by Percentile
# ===========================================================================

cat("\nGenerating income comparison bar graphs...\n")

# Prepare data for income comparison plots
prepare_income_comparison_data <- function(year_val) {
  bind_rows(
    p25_data %>% filter(year == year_val) %>%
      summarise(NoUniv = mean(`No University`), Univ = mean(University)) %>%
      mutate(Percentile = "P25"),
    median_data %>% filter(year == year_val) %>%
      summarise(NoUniv = mean(`No University`), Univ = mean(University)) %>%
      mutate(Percentile = "Median"),
    p75_data %>% filter(year == year_val) %>%
      summarise(NoUniv = mean(`No University`), Univ = mean(University)) %>%
      mutate(Percentile = "P75"),
    p90_data %>% filter(year == year_val) %>%
      summarise(NoUniv = mean(`No University`), Univ = mean(University)) %>%
      mutate(Percentile = "P90")
  ) %>%
    pivot_longer(cols = c(NoUniv, Univ), names_to = "Education", values_to = "Income") %>%
    mutate(
      Percentile = factor(Percentile, levels = c("P25", "Median", "P75", "P90")),
      Education = factor(Education, levels = c("NoUniv", "Univ"),
                        labels = c("No University", "University"))
    )
}

income_2017 <- prepare_income_comparison_data(2017)
income_2024 <- prepare_income_comparison_data(2024)

# Create income comparison plot function (English)
create_income_comparison_plot <- function(data, year, title, subtitle, filename) {
  p <- ggplot(data, aes(x = Percentile, y = Income, fill = Education)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = format(round(Income), big.mark = ",")),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 4.5) +
    scale_fill_manual(values = c("No University" = "#6B7B8C", "University" = "#A3B1BF")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Percentile",
      y = "Hourly Income (CLP/hour)",
      fill = "Education"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14, color = "gray40"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(data$Income) * 1.15)

  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
  p
}

# Create income comparison plot function (Spanish)
create_income_comparison_plot_es <- function(data, year, title, subtitle, filename) {
  data_es <- data %>%
    mutate(Educacion = factor(Education,
                              levels = c("No University", "University"),
                              labels = c("Sin Universidad", "Universidad")))

  p <- ggplot(data_es, aes(x = Percentile, y = Income, fill = Educacion)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = format(round(Income), big.mark = ",")),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 4.5) +
    scale_fill_manual(values = c("Sin Universidad" = "#6B7B8C", "Universidad" = "#A3B1BF")) +
    scale_x_discrete(labels = c("P25" = "P25", "Median" = "Mediana", "P75" = "P75", "P90" = "P90")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Percentil",
      y = "Ingreso por Hora (CLP/hora)",
      fill = "Educación"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 14, color = "gray40"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, max(data$Income) * 1.15)

  ggsave(filename, p, width = 10, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
  p
}

# Generate English plots
p_income_2017_en <- create_income_comparison_plot(
  income_2017, 2017,
  "Hourly Labor Income by Education (2017)",
  "Average across age cohorts, inflation-adjusted to 2024 CLP",
  "output/figures/income_by_education_2017.png"
)

p_income_2024_en <- create_income_comparison_plot(
  income_2024, 2024,
  "Hourly Labor Income by Education (2024)",
  "Average across age cohorts",
  "output/figures/income_by_education_2024.png"
)

# Generate Spanish plots
p_income_2017_es <- create_income_comparison_plot_es(
  income_2017, 2017,
  "Ingreso Laboral por Hora según Educación (2017)",
  "Promedio entre cohortes de edad, ajustado por inflación a CLP 2024",
  "output/figures/income_by_education_2017_es.png"
)

p_income_2024_es <- create_income_comparison_plot_es(
  income_2024, 2024,
  "Ingreso Laboral por Hora según Educación (2024)",
  "Promedio entre cohortes de edad",
  "output/figures/income_by_education_2024_es.png"
)

# Save updated table to Excel
addWorksheet(wb, "Premium_Summary")
writeData(wb, "Premium_Summary", table17)
saveWorkbook(wb, "output/tables/labor_income_tables.xlsx", overwrite = TRUE)
cat("Updated: output/tables/labor_income_tables.xlsx\n")

cat("\nDone!\n")
