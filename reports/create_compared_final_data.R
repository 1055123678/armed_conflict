library(dplyr)
library(janitor)
library(purrr)
library(readr)
library(tidyr)

# Convert one World Bank mortality file from wide to long format.
prepare_mortality <- function(path, value_name) {
  read_csv(path, show_col_types = FALSE) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      values_to = value_name,
      names_transform = list(year = ~ as.integer(sub("^X", "", .x)))
    ) |>
    select(iso, year, all_of(value_name))
}

mortality_files <- c(
  maternal_mortality = "data/raw/maternal_mortality.csv",
  infant_mortality = "data/raw/infant_mortality.csv",
  neonatal_mortality = "data/raw/neonatal_mortality.csv",
  under5_mortality = "data/raw/under5_mortality.csv"
)

mortality_data <- imap(
  mortality_files,
  ~ prepare_mortality(.x, .y)
) |>
  reduce(full_join, by = c("iso", "year"))

disaster_data <- read_csv("data/raw/disaster.csv", show_col_types = FALSE) |>
  clean_names() |>
  filter(
    between(year, 2000, 2019),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  distinct(year, iso, disaster_type) |>
  mutate(value = 1L) |>
  pivot_wider(
    names_from = disaster_type,
    values_from = value,
    values_fill = 0L
  ) |>
  clean_names() |>
  select(year, iso, earthquake, drought)

conflict_data <- read_csv("data/raw/conflict.csv", show_col_types = FALSE) |>
  group_by(iso, year) |>
  summarise(total_deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  transmute(
    iso,
    year = year + 1L,
    armed_conflict = as.integer(total_deaths >= 25)
  ) |>
  filter(between(year, 2000, 2019))

covariates <- read_csv("data/raw/covariates.csv", show_col_types = FALSE)

compared_final_data <- mortality_data |>
  left_join(disaster_data, by = c("iso", "year")) |>
  left_join(conflict_data, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  mutate(
    across(c(earthquake, drought, armed_conflict), ~ replace_na(.x, 0L))
  ) |>
  arrange(iso, year)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(compared_final_data, "data/processed/compared_final_data.csv", na = "NA")
