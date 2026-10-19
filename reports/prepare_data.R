library(tidyr)
library(dplyr)
library(janitor)

# Read mortality data
maternal_mortality <- read.csv("./data/raw/maternal_mortality.csv")
infant_mortality <- read.csv("./data/raw/infant_mortality.csv")
neonatal_mortality <- read.csv("./data/raw/neonatal_mortality.csv")
under5_mortality <- read.csv("./data/raw/under5_mortality.csv")

# General function for all mortality data
prepare_mortality <- function(data, value_name) {
  data |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      values_to = value_name
    ) |>
    mutate(
      year = as.integer(sub("X", "", year))
    ) |>
    select(
      iso,
      year,
      all_of(value_name)
    )
}

maternal_mortality_long <- prepare_mortality(
  maternal_mortality,
  "maternal_mortality"
)

infant_mortality_long <- prepare_mortality(
  infant_mortality,
  "infant_mortality"
)

neonatal_mortality_long <- prepare_mortality(
  neonatal_mortality,
  "neonatal_mortality"
)

under5_mortality_long <- prepare_mortality(
  under5_mortality,
  "under5_mortality"
)


#Prepare disaster data
disaster <- read.csv("./data/raw/disaster.csv")

disaster <- disaster |>
  clean_names()

disaster <- disaster |>
  filter(year >= 2000 & year <= 2019)

disaster <- disaster |>
  filter(disaster_type %in% c("Earthquake", "Drought"))

disaster <- disaster |>
  select(year, iso, disaster_type)

disaster <- disaster |>
  mutate(value = 1)

disaster <- disaster |>
  pivot_wider(
    names_from = disaster_type,
    values_from = value,
    values_fill = 0,
    values_fn = max
  )

disaster <- disaster |>
  clean_names()

disaster <- disaster |>
  select(year, iso, earthquake, drought)

#Prepare conflict data
conflict <- read.csv("./data/raw/conflict.csv")

conflict <- conflict |>
  group_by(iso, year) |>
  summarise(
    total_deaths = sum(best, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    armed_conflict = ifelse(total_deaths >= 25, 1, 0),
    year = year + 1
  ) |>
  filter(year >= 2000) |>
  select(year, iso, armed_conflict)

conflict

covariates <- read.csv("./data/raw/covariates.csv")


final_data <- maternal_mortality_long |>
  left_join(infant_mortality_long, by = c("iso", "year")) |>
  left_join(neonatal_mortality_long, by = c("iso", "year")) |>
  left_join(under5_mortality_long, by = c("iso", "year")) |>
  left_join(disaster, by = c("iso", "year")) |>
  left_join(conflict, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year"))

names(final_data)
dir.create("./data/processed", showWarnings = FALSE)

write.csv(
  final_data,
  "./data/processed/final_data.csv",
  row.names = FALSE
)
