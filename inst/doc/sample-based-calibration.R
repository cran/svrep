## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# calibrate_to_estimate(
#   rep_design = rep_design,
#   estimate = vector_of_control_totals,
#   vcov_estimate = variance_covariance_matrix_for_controls,
#   cal_formula = ~ CALIBRATION_VARIABLE_1 + CALIBRATION_VARIABLE_2 + ...,
# )

## ----eval=FALSE---------------------------------------------------------------
# calibrate_to_sample(
#   primary_rep_design = primary_rep_design,
#   control_rep_design = control_rep_design
#   cal_formula = ~ CALIBRATION_VARIABLE_1 + CALIBRATION_VARIABLE_2 + ...,
# )

## ----setup--------------------------------------------------------------------
# Load the data
library(svrep)
data("lou_vax_survey")

# Inspect the first few rows
head(lou_vax_survey) |> knitr::kable()

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(
  library(survey)
)

lou_vax_survey_rep <- svydesign(
  data = lou_vax_survey,
  ids = ~ 1, weights = ~ SAMPLING_WEIGHT
) |> 
  as.svrepdesign(type = "JK1", mse = TRUE)

## ----echo=FALSE---------------------------------------------------------------
lou_vax_survey_rep

## -----------------------------------------------------------------------------
# Conduct nonresponse weighting adjustment

nr_adjusted_design <- lou_vax_survey_rep |>
  redistribute_weights(
    reduce_if = RESPONSE_STATUS == "Nonrespondent",
    increase_if = RESPONSE_STATUS == "Respondent"
  ) |>
  subset(RESPONSE_STATUS == "Respondent")

# Inspect the result of the adjustment
rbind(
  'Original' = summarize_rep_weights(lou_vax_survey_rep, type = 'overall'),
  'NR-adjusted' = summarize_rep_weights(nr_adjusted_design, type = 'overall')
)[,c("nrows", "rank", "avg_wgt_sum", "sd_wgt_sums")]

## ----results='hide'-----------------------------------------------------------
data("lou_pums_microdata")

## -----------------------------------------------------------------------------
# Inspect some of the rows/columns of data ----
tail(lou_pums_microdata, n = 5) |> 
  dplyr::select(AGE, SEX, RACE_ETHNICITY, EDUC_ATTAINMENT) |>
  knitr::kable()

## -----------------------------------------------------------------------------
# Convert to a survey design object ----
  pums_rep_design <- svrepdesign(
      data = lou_pums_microdata,
      weights = ~ PWGTP,
      repweights = "PWGTP\\d{1,2}",
      type = "successive-difference",
      variables = ~ AGE + SEX + RACE_ETHNICITY + EDUC_ATTAINMENT,
      mse = TRUE
    )

  pums_rep_design

## -----------------------------------------------------------------------------
# Subset to only include adults
pums_rep_design <- pums_rep_design |> subset(AGE >= 18)

## -----------------------------------------------------------------------------
suppressPackageStartupMessages(
  library(dplyr)
)

# Check that variables match across data sources ----
  pums_rep_design$variables |>
    dplyr::distinct(RACE_ETHNICITY)

  setdiff(lou_vax_survey_rep$variables$RACE_ETHNICITY,
          pums_rep_design$variables$RACE_ETHNICITY)
  setdiff(lou_vax_survey_rep$variables$SEX,
          pums_rep_design$variables$SEX)
  setdiff(lou_vax_survey_rep$variables$EDUC_ATTAINMENT,
          pums_rep_design$variables$EDUC_ATTAINMENT)

## -----------------------------------------------------------------------------
# Estimates from the control survey (ACS)
svymean(
  design = pums_rep_design,
  x = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT
)

# Estimates from the primary survey (Louisville vaccination survey)
svymean(
  design = nr_adjusted_design,
  x = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT
)

## -----------------------------------------------------------------------------
acs_control_totals <- svytotal(
  x = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT,
  design = pums_rep_design
)

control_totals_for_raking <- list(
  'estimates' = coef(acs_control_totals),
  'variance-covariance' = vcov(acs_control_totals)
)

# Inspect point estimates
control_totals_for_raking$estimates

# Inspect a few rows of the control totals' variance-covariance matrix
control_totals_for_raking$`variance-covariance`[5:8,5:8] |>
  `colnames<-`(NULL)

## -----------------------------------------------------------------------------
svytotal(x = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT,
         design = nr_adjusted_design)

## -----------------------------------------------------------------------------
raked_design <- calibrate_to_estimate(
  rep_design = nr_adjusted_design,
  estimate = control_totals_for_raking$estimates,
  vcov_estimate = control_totals_for_raking$`variance-covariance`,
  cal_formula = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT,
  calfun = survey::cal.raking, # Required for raking
  epsilon = 1e-9
)

## -----------------------------------------------------------------------------
# Estimated totals after calibration
svytotal(x = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT,
         design = raked_design)

# Matches the control totals!
cbind(
  'total' = control_totals_for_raking$estimates,
  'SE' = control_totals_for_raking$`variance-covariance` |>
    diag() |> sqrt()
)

## -----------------------------------------------------------------------------
estimates_by_design <- svyby_repwts(
  rep_designs = list(
    "NR-adjusted" = nr_adjusted_design,
    "Raked" = raked_design
  ),
  FUN = svytotal,
  formula = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT
)

t(estimates_by_design[,-1]) |>
  knitr::kable()

## -----------------------------------------------------------------------------
raked_design_opsomer_erciulescu <- calibrate_to_sample(
  primary_rep_design = nr_adjusted_design,
  control_rep_design = pums_rep_design,
  cal_formula = ~ RACE_ETHNICITY + SEX + EDUC_ATTAINMENT,
  calfun = survey::cal.raking,
  epsilon = 1e-9
)

## -----------------------------------------------------------------------------
estimates_by_design <- svyby_repwts(
  rep_designs = list(
    "calibrate_to_estimate()" = raked_design,
    "calibrate_to_sample()" = raked_design_opsomer_erciulescu
  ),
  FUN = svytotal,
  formula = ~ VAX_STATUS + RACE_ETHNICITY + SEX + EDUC_ATTAINMENT
)

t(estimates_by_design[,-1]) |>
  knitr::kable()

## -----------------------------------------------------------------------------
# Create matching post-stratification variable in both datasets
  nr_adjusted_design <- nr_adjusted_design |>
    transform(POSTSTRATUM = interaction(RACE_ETHNICITY, SEX, EDUC_ATTAINMENT,
                                        sep = "|"))

  pums_rep_design <- pums_rep_design |>
    transform(POSTSTRATUM = interaction(RACE_ETHNICITY, SEX, EDUC_ATTAINMENT,
                                        sep = "|"))
  
  levels(pums_rep_design$variables$POSTSTRATUM) <- levels(
    nr_adjusted_design$variables$POSTSTRATUM
  )

# Estimate control totals
  acs_control_totals <- svytotal(
    x = ~ POSTSTRATUM,
    design = pums_rep_design
  )
  
  poststratification_totals <- list(
    'estimate' = coef(acs_control_totals),
    'variance-covariance' = vcov(acs_control_totals)
  )

# Inspect the control totals
  poststratification_totals$estimate |>
    as.data.frame() |>
    `colnames<-`('estimate') |>
    knitr::kable()

## -----------------------------------------------------------------------------
# Post-stratify the design using the estimates
poststrat_design_fuller <- calibrate_to_estimate(
  rep_design = nr_adjusted_design,
  estimate = poststratification_totals$estimate,
  vcov_estimate = poststratification_totals$`variance-covariance`,
  cal_formula = ~ POSTSTRATUM, # Specify the post-stratification variable
  calfun = survey::cal.linear # This option is required for post-stratification
)

## -----------------------------------------------------------------------------
# Post-stratify the design using the two samples
poststrat_design_opsomer_erciulescu <- calibrate_to_sample(
  primary_rep_design = nr_adjusted_design,
  control_rep_design = pums_rep_design,
  cal_formula = ~ POSTSTRATUM, # Specify the post-stratification variable
  calfun = survey::cal.linear # This option is required for post-stratification
)

## -----------------------------------------------------------------------------
estimates_by_design <- svyby_repwts(
  rep_designs = list(
    "calibrate_to_estimate()" = poststrat_design_fuller,
    "calibrate_to_sample()" = poststrat_design_opsomer_erciulescu
  ),
  FUN = svymean,
  formula = ~ VAX_STATUS + RACE_ETHNICITY + SEX + EDUC_ATTAINMENT
)

t(estimates_by_design[,-1]) |>
  knitr::kable()

## -----------------------------------------------------------------------------
# Randomly select which columns will be assigned to each set of perturbed control totals
dimension_of_control_totals <- length(poststratification_totals$estimate)

columns_to_perturb <- sample(x = 1:ncol(nr_adjusted_design$repweights),
                             size = dimension_of_control_totals)

print(columns_to_perturb)

# Perform the calibration
poststratified_design <- calibrate_to_estimate(
  rep_design = nr_adjusted_design,
  estimate = poststratification_totals$estimate,
  vcov_estimate = poststratification_totals$`variance-covariance`,
  cal_formula = ~ POSTSTRATUM,
  calfun = survey::cal.linear,
  col_selection = columns_to_perturb # Specified for reproducibility
)

## -----------------------------------------------------------------------------
poststratified_design$perturbed_control_cols

## -----------------------------------------------------------------------------
# Randomly match the primary replicates to control replicates
set.seed(1999)

column_matching <- rep(NA, times = ncol(nr_adjusted_design$repweights))
column_matching[sample(x = 1:1000, size = 80)] <- 1:80

str(column_matching)

# Perform the calibration
poststratified_design <- calibrate_to_sample(
  primary_rep_design = nr_adjusted_design,
  control_rep_design = pums_rep_design,
  cal_formula = ~ POSTSTRATUM,
  calfun = survey::cal.linear,
  control_col_matches = column_matching
)

## -----------------------------------------------------------------------------
str(poststratified_design$control_column_matches)

