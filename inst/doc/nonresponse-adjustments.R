## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(dplyr) # For data manipulation
library(survey) # For complex survey analysis
library(srvyr) # For complex survey analysis with dplyr syntax
library(svrep)

## -----------------------------------------------------------------------------
# Load and inspect the data
data("lou_vax_survey", package = 'svrep')
head(lou_vax_survey)
colnames(lou_vax_survey)

## -----------------------------------------------------------------------------
lou_vax_survey |> count(RESPONSE_STATUS) |> mutate(pct = n/sum(n))

## -----------------------------------------------------------------------------
# Describe the survey design
lou_vax_survey <- svydesign(ids = ~ 1, weights = ~ SAMPLING_WEIGHT,
                            data = lou_vax_survey)

print(lou_vax_survey)

# Create appropriate replicate weights
lou_vax_survey <- lou_vax_survey |>
  as_bootstrap_design(replicates = 100, mse = TRUE,
                      type = "Rao-Wu-Yue-Beaumont")

print(lou_vax_survey)

## -----------------------------------------------------------------------------
lou_vax_survey <- lou_vax_survey |> as_survey()

print(lou_vax_survey)

## -----------------------------------------------------------------------------
# Weights before adjustment
lou_vax_survey |>
  group_by(RESPONSE_STATUS) |>
  cascade(
    `Sum of Weights` = sum(cur_svy_wts()),
    .fill = "TOTAL"
  )

## -----------------------------------------------------------------------------
# Conduct a basic nonresponse adjustment
nr_adjusted_survey <- lou_vax_survey |>
  redistribute_weights(
    reduce_if = RESPONSE_STATUS == "Nonrespondent",
    increase_if = RESPONSE_STATUS == "Respondent"
  )

## -----------------------------------------------------------------------------
# Check the sum of full-sample weights by response status
nr_adjusted_survey |>
  group_by(RESPONSE_STATUS) |>
  cascade(
    `Sum of Weights` = sum(cur_svy_wts()),
    .fill = "TOTAL"
  )

## -----------------------------------------------------------------------------
# Check sums of replicate weights by response status
nr_adjusted_survey |>
  summarize_rep_weights(
    type = "specific",
    by = "RESPONSE_STATUS"
  ) |> 
  arrange(Rep_Column, RESPONSE_STATUS) |>
  head(10)

## -----------------------------------------------------------------------------
lou_vax_survey |>
  group_by(RACE_ETHNICITY) |>
  summarize(Response_Rate = mean(RESPONSE_STATUS == "Respondent"),
            Sample_Size = n(),
            n_Respondents = sum(RESPONSE_STATUS == "Respondent"))

## -----------------------------------------------------------------------------
nr_adjusted_survey <- lou_vax_survey |>
  redistribute_weights(
    reduce_if = RESPONSE_STATUS == "Nonrespondent",
    increase_if = RESPONSE_STATUS == "Respondent",
    by = c("RACE_ETHNICITY")
  )

## -----------------------------------------------------------------------------
# Fit a response propensity model
response_propensity_model <- lou_vax_survey |>
  mutate(IS_RESPONDENT = ifelse(RESPONSE_STATUS == "Respondent", 1, 0)) |>
  svyglm(formula = IS_RESPONDENT ~ RACE_ETHNICITY + EDUC_ATTAINMENT,
         family = quasibinomial(link = 'logit'))

# Predict response propensities for individual cases
lou_vax_survey <- lou_vax_survey |>
  mutate(
    RESPONSE_PROPENSITY = predict(response_propensity_model,
                                  newdata = cur_svy(),
                                  type = "response")
  )

# Divide sample into propensity classes
lou_vax_survey <- lou_vax_survey |>
  mutate(PROPENSITY_CELL = ntile(x = RESPONSE_PROPENSITY, n = 5))

lou_vax_survey |>
    group_by(PROPENSITY_CELL) |>
    summarize(n = n(),
              min = min(RESPONSE_PROPENSITY),
              mean = mean(RESPONSE_PROPENSITY),
              max = max(RESPONSE_PROPENSITY))

# Redistribute weights by propensity class
nr_adjusted_survey <- lou_vax_survey |>
  redistribute_weights(
    reduce_if = RESPONSE_STATUS == "Nonrespondent",
    increase_if = RESPONSE_STATUS == "Respondent",
    by = "PROPENSITY_CELL"
  )

# Inspect weights before adjustment

lou_vax_survey |>
  summarize_rep_weights(type = "specific",
                        by = c("PROPENSITY_CELL")) |>
  arrange(Rep_Column, PROPENSITY_CELL) |>
  select(PROPENSITY_CELL, Rep_Column,
         N_NONZERO, SUM) |>
  head(10)

# Inspect weights after adjustment
nr_adjusted_survey |>
  summarize_rep_weights(type = "specific",
                        by = c("PROPENSITY_CELL", "RESPONSE_STATUS")) |>
  arrange(Rep_Column, PROPENSITY_CELL, RESPONSE_STATUS) |>
  select(PROPENSITY_CELL, RESPONSE_STATUS, Rep_Column,
         N_NONZERO, SUM) |>
  head(10)

## -----------------------------------------------------------------------------
data_frame_with_nr_adjusted_weights <- nr_adjusted_survey |>
  as_data_frame_with_weights(
    full_wgt_name = "NR_ADJ_WGT",
    rep_wgt_prefix = "NR_ADJ_REP_WGT_"
  )

# Preview first few column names
colnames(data_frame_with_nr_adjusted_weights) |> head(12)

## ---- eval=FALSE--------------------------------------------------------------
#  # Write the data to a CSV file
#  write.csv(
#    x = data_frame_with_nr_adjusted_weights,
#    file = "survey-data-with-nonresponse-adjusted-weights.csv"
#  )

