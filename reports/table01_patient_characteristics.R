library(dplyr)
library(readr)
library(flextable)
library(officer)

data <- read_csv("data/processed/final_data.csv")

# Define groups
data <- data |>
  mutate(
    conflict_group = factor(
      armed_conflict,
      levels = c(0, 1),
      labels = c("No conflict", "Conflict")
    )
  )
# Continuous variables
continuous_vars <- c(
  "maternal_mortality",
  "infant_mortality",
  "neonatal_mortality",
  "under5_mortality",
  "gdp_1000",
  "pop_dens",
  "urban",
  "age_dep",
  "male_edu",
  "temp",
  "rainfall_1000"
)
# Binary variables
binary_vars <- c(
  "oecd",
  "earthquake",
  "drought"
)

# Summarize continuous variables
summarize_continuous <- function(df, var) {
  x <- df[[var]]
  n <- sum(!is.na(x))

  if (n == 0) return("NA")

  sprintf(
    "%.2f (%.2f)",
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

# Summarize binary variables: number and percentage of 1
summarize_binary <- function(df, var) {
  x <- df[[var]]
  n <- sum(!is.na(x))

  if (n == 0) return("NA")

  sprintf(
    "%d (%.1f%%)",
    sum(x == 1, na.rm = TRUE),
    100 * mean(x == 1, na.rm = TRUE)
  )
}

# Define subsets
groups <- list(
  "Overall" = data,
  "No conflict" = filter(data, armed_conflict == 0),
  "Conflict" = filter(data, armed_conflict == 1)
)

# Create rows
make_row <- function(var, type) {
  result <- data.frame(Variable = var)

  for (group_name in names(groups)) {
    df <- groups[[group_name]]

    if (type == "continuous") {
      result[[group_name]] <- summarize_continuous(df, var)
    } else {
      result[[group_name]] <- summarize_binary(df, var)
    }

    result[[paste0(group_name, " missing")]] <-
      sum(is.na(df[[var]]))
  }

  result
}

# Combine all variables
table_data <- bind_rows(
  lapply(continuous_vars, make_row, type = "continuous"),
  lapply(binary_vars, make_row, type = "binary")
)

# Add observation counts
n_total <- nrow(data)
n_no <- sum(data$armed_conflict == 0)
n_yes <- sum(data$armed_conflict == 1)

observation_row <- data.frame(
  Variable = "Observations (N)",
  Overall = as.character(n_total),
  `Overall missing` = NA_integer_,
  `No conflict` = as.character(n_no),
  `No conflict missing` = NA_integer_,
  Conflict = as.character(n_yes),
  `Conflict missing` = NA_integer_,
  check.names = FALSE
)

table_data <- bind_rows(observation_row, table_data)

# Reorder columns
table_data <- table_data |>
  select(
    Variable,
    Overall, `Overall missing`,
    `No conflict`, `No conflict missing`,
    Conflict, `Conflict missing`
  )

table_data <- table_data |>
  mutate(
    Variable = recode(
      Variable,
      "Observations (N)" = "Number of observations",
      "maternal_mortality" = "Maternal mortality",
      "infant_mortality" = "Infant mortality",
      "neonatal_mortality" = "Neonatal mortality",
      "under5_mortality" = "Under-5 mortality",
      "gdp_1000" = "GDP per capita (1000s)",
      "pop_dens" = "Population density",
      "urban" = "Urban population",
      "age_dep" = "Age dependency ratio",
      "male_edu" = "Male education",
      "temp" = "Temperature",
      "rainfall_1000" = "Rainfall (1000s)",
      "oecd" = "OECD membership (Yes)",
      "earthquake" = "Earthquake (Yes)",
      "drought" = "Drought (Yes)"
    )
  )

# Create formatted table
library(officer)

ft <- flextable(table_data) |>
  set_header_labels(
    Variable = "Characteristic",
    Overall = "Overall",
    `Overall missing` = "Missing",
    `No conflict` = "No conflict",
    `No conflict missing` = "Missing",
    Conflict = "Conflict",
    `Conflict missing` = "Missing"
  ) |>
  theme_booktabs() |>
  autofit() |>
  fontsize(size = 9, part = "all")

ft <- add_footer_lines(
  ft,
  values = paste(
    "Values are presented as mean (SD) for continuous variables and n (%) for binary variables.",
    "Missing columns report the number of missing observations.",
    "Observations represent country-years."
  )
)

landscape_section <- prop_section(
  page_size = page_size(orient = "landscape"),
  page_margins = page_mar(
    top = 0.5, bottom = 0.5,
    left = 0.5, right = 0.5
  ),
  type = "continuous"
)

save_as_docx(
  "Table 1. Study characteristics by armed conflict status" = ft,
  path = "reports/table01_patient_characteristics.docx",
  pr_section = landscape_section
)

#Peer review: change variable names on the table more readable