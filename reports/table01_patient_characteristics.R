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

# Create formatted table
ft <- flextable(table_data) |>
  theme_booktabs() |>
  autofit()

ft <- set_caption(
  ft,
  "Table 1. Characteristics of country-year observations by armed conflict status"
)

# Save Word document
dir.create("reports", showWarnings = FALSE)

save_as_docx(
  ft,
  path = "reports/table01_patient_characteristics.docx"
)

