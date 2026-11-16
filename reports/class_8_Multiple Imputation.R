library(dplyr)
library(plm)
library(mice)
library(gt)

# Variables
outcomes <- c(
  "maternal_mortality", "under5_mortality",
  "infant_mortality", "neonatal_mortality"
)

predictors <- c(
  "armed_conflict", "log_gdp", "oecd", "pop_dens",
  "urban", "age_dep", "male_edu", "temp",
  "rainfall_1000", "earthquake", "drought"
)

impute_vars <- c(
  outcomes, "log_gdp", "pop_dens", "urban",
  "male_edu", "temp", "rainfall_1000"
)

required <- c("iso", "year", outcomes, predictors)

stopifnot(all(required %in% names(finaldata)))

data <- finaldata |>
  select(all_of(required))

# Complete-Case Regression
cc_models <- lapply(outcomes, function(y) {
  plm(
    reformulate(predictors, response = y),
    data = data,
    index = c("iso", "year"),
    model = "within",
    effect = "twoways"
  )
})

names(cc_models) <- outcomes


# Multiple Imputation
midata <- data |>
  mutate(iso_num = as.numeric(factor(iso))) |>
  select(-iso)

mice0 <- mice(midata, maxit = 0, printFlag = FALSE)

meth <- mice0$method
pred <- mice0$predictorMatrix

meth[impute_vars] <- "2l.lmer"
meth["iso_num"] <- ""

pred[impute_vars, "iso_num"] <- -2
pred["iso_num", ] <- 0

mice.multi.out <- mice(
  midata,
  m = 10,
  maxit = 20,
  method = meth,
  predictorMatrix = pred,
  seed = 100
)

# Convergence plot
plot(mice.multi.out)

# Check imputation warnings
mice.multi.out$loggedEvents

# MI Regression
completed_data <- mice::complete(mice.multi.out, "all")

mi_models <- lapply(outcomes, function(y) {

  f <- reformulate(
    c(predictors, "factor(iso_num)", "factor(year)"),
    response = y,
    intercept = FALSE
  )

  fits <- lapply(completed_data, function(d) {
    lm(f, data = d)
  })

  pool(as.mira(fits))
})

names(mi_models) <- outcomes

# Extract Coefficients and 95% CI 
cc_results <- bind_rows(lapply(outcomes, function(y) {

  b <- coef(cc_models[[y]])
  se <- sqrt(diag(vcov(cc_models[[y]])))

  tibble(
    Outcome = y,
    Predictor = names(b),
    CC = sprintf(
      "%.3f (%.3f, %.3f)",
      b, b - 1.96 * se, b + 1.96 * se
    )
  )
}))

mi_results <- bind_rows(lapply(outcomes, function(y) {

  s <- summary(mi_models[[y]], conf.int = TRUE)

  tibble(
    Outcome = y,
    Predictor = s$term,
    MI = sprintf(
      "%.3f (%.3f, %.3f)",
      s$estimate, s$`2.5 %`, s$`97.5 %`
    )
  )
}))

# Final Comparison Table
comparison_table <- full_join(
  cc_results, mi_results,
  by = c("Outcome", "Predictor")
) |>
  filter(Predictor %in% predictors) |>
  arrange(
    match(Outcome, outcomes),
    match(Predictor, predictors)
  )


comparison_wide <- full_join(
  cc_results, mi_results,
  by = c("Outcome", "Predictor")
) |>
  filter(Predictor %in% predictors) |>
  pivot_longer(
    cols = c(CC, MI),
    names_to = "Method",
    values_to = "Estimate"
  ) |>
  mutate(
    Outcome = recode(
      Outcome,
      maternal_mortality = "Maternal",
      under5_mortality = "Under-five",
      infant_mortality = "Infant",
      neonatal_mortality = "Neonatal"
    ),
    Predictor = recode(
      Predictor,
      armed_conflict = "Armed Conflict",
      log_gdp = "Log GDP",
      oecd = "OECD Membership",
      pop_dens = "Population Density",
      urban = "Urbanization",
      age_dep = "Age Dependency",
      male_edu = "Male Education",
      temp = "Temperature",
      rainfall_1000 = "Rainfall",
      earthquake = "Earthquake",
      drought = "Drought"
    ),
    Column = paste(Outcome, Method, sep = "_")
  ) |>
  select(Predictor, Column, Estimate) |>
  pivot_wider(
    names_from = Column,
    values_from = Estimate
  )

# Keep columns in desired order
comparison_wide <- comparison_wide |>
  select(
    Predictor,
    Maternal_CC, Maternal_MI,
    `Under-five_CC`, `Under-five_MI`,
    Infant_CC, Infant_MI,
    Neonatal_CC, Neonatal_MI
  )

# Create table with grouped column headings
final_table <- comparison_wide |>
  gt::gt() |>
  gt::tab_header(
    title = "Association Between Armed Conflict and Mortality",
    subtitle = "Complete-Case vs. Multiple Imputation Analysis"
  ) |>
  gt::tab_spanner(
    label = "Maternal Mortality",
    columns = c(Maternal_CC, Maternal_MI)
  ) |>
  gt::tab_spanner(
    label = "Under-five Mortality",
    columns = c(`Under-five_CC`, `Under-five_MI`)
  ) |>
  gt::tab_spanner(
    label = "Infant Mortality",
    columns = c(Infant_CC, Infant_MI)
  ) |>
  gt::tab_spanner(
    label = "Neonatal Mortality",
    columns = c(Neonatal_CC, Neonatal_MI)
  ) |>
  gt::cols_label(
    Predictor = "Predictor",
    Maternal_CC = "CC",
    Maternal_MI = "MI",
    `Under-five_CC` = "CC",
    `Under-five_MI` = "MI",
    Infant_CC = "CC",
    Infant_MI = "MI",
    Neonatal_CC = "CC",
    Neonatal_MI = "MI"
  )
final_table
dir.create("output", showWarnings = FALSE)
gt::gtsave(
  final_table,
  "reports/table03_comparison_CC_MI.html"
)