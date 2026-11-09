library(dplyr)
library(readr)
library(plm)
library(stargazer)

finaldata <- read_csv("data/processed/final_data.csv") |>
  mutate(log_gdp = if_else(gdp_1000 > 0, log(gdp_1000), NA_real_))

preds <- ~ armed_conflict + log_gdp + oecd + pop_dens +
  urban + age_dep + male_edu + temp + rainfall_1000 +
  earthquake + drought

fit_model <- function(outcome) {
  plm(
    update.formula(preds, paste(outcome, "~ .")),
    data = finaldata,
    index = c("iso", "year"),
    model = "within",
    effect = "twoways"
  )
}

maternal_mod <- fit_model("maternal_mortality")
under5_mod   <- fit_model("under5_mortality")
infant_mod   <- fit_model("infant_mortality")
neonatal_mod <- fit_model("neonatal_mortality")


stargazer(
  maternal_mod, under5_mod, infant_mod, neonatal_mod,
  type = "html",
  out = "table02_Association_Between_Armed_Conflict_and_Maternal_and_Child_Mortality.html",
  title = "Table 2. Association Between Armed Conflict and Maternal and Child Mortality: Adjusted Regression Coefficients and 95% Confidence Intervals",
  column.labels = c("Maternal", "Under-five", "Infant", "Neonatal"),
  dep.var.labels.include = FALSE,
  digits = 3
)