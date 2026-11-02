library(dplyr)
library(readr)
library(ggplot2)

# Load prepared data
data <- read_csv("data/processed/final_data.csv")

# Select data from 2000 to 2017
mortality_data <- data |>
  select(iso, year, maternal_mortality) |>
  filter(year >= 2000, year <= 2017) |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(
    diff_matmor = maternal_mortality -
      maternal_mortality[year == 2000][1]
  ) |>
  ungroup()

# Identify countries with an increase from 2000 to 2017
increased_countries <- mortality_data |>
  filter(year %in% c(2000, 2017)) |>
  group_by(iso) |>
  filter(
    n_distinct(year) == 2,
    all(!is.na(maternal_mortality))
  ) |>
  summarise(
    change = maternal_mortality[year == 2017] -
      maternal_mortality[year == 2000],
    .groups = "drop"
  ) |>
  filter(change > 0)

# Keep all years for selected countries
plot_data <- mortality_data |>
  filter(iso %in% increased_countries$iso)

# Check selected countries
print(increased_countries)

# Create line graph
fig1 <- ggplot(
  plot_data,
  aes(
    x = year,
    y = maternal_mortality,
    color = iso,
    group = iso
  )
) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 1.5, na.rm = TRUE) +
  labs(
    title = "Maternal Mortality Trends, 2000–2017",
    subtitle = "Countries with increased maternal mortality",
    x = "Year",
    y = "Maternal mortality ratio",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(fig1)

dir.create("figures", showWarnings = FALSE)

ggsave(
  filename = "figures/fig01_maternal_mortality.png",
  plot = fig1,
  width = 10,
  height = 6,
  dpi = 300
)