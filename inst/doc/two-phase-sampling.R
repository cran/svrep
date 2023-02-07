## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE, warning=FALSE, echo=FALSE--------------------------
library(dplyr) # For data manipulation
library(survey) # For complex survey analysis
library(srvyr) # For complex survey analysis with dplyr syntax
library(svrep)

## -----------------------------------------------------------------------------
data('library_multistage_sample', package = 'svrep')

# Load first-phase sample
  twophase_sample <- library_multistage_sample

# Select second-phase sample
  set.seed(2020)
  
  twophase_sample[['SECOND_PHASE_SELECTION']] <- sampling::srswor(
    n = 100,
    N = nrow(twophase_sample)
  ) |> as.logical()

## -----------------------------------------------------------------------------
# Declare survey design
  twophase_design <- twophase(
    method = "full",
    data = twophase_sample,
    # Identify the subset of first-phase elements
    # which were selected into the second-phase sample
    subset = ~ SECOND_PHASE_SELECTION,
    # Describe clusters, probabilities, and population sizes
    # at each phase of sampling
    id = list(~ PSU_ID + SSU_ID,
              ~ 1),
    probs = list(~ PSU_SAMPLING_PROB + SSU_SAMPLING_PROB,
                 NULL),
    fpc = list(~ PSU_POP_SIZE + SSU_POP_SIZE,
               NULL)
  )

## ---- warning=FALSE-----------------------------------------------------------
# Obtain a generalized bootstrap replicates
# based on 
#   - The phase 1 estimator is the usual variance estimator
#     for stratified multistage simple random sampling
#   - The phase 2 estimator is the usual variance estimator
#     for single-stage simple random sampling

twophase_boot_design <- as_gen_boot_design(
  design = twophase_design,
  variance_estimator = list(
    "Phase 1" = "Stratified Multistage SRS",
    "Phase 2" = "Ultimate Cluster"
  ),
  replicates = 1000
)

## -----------------------------------------------------------------------------
twophase_boot_design |> svymean(x = ~ LIBRARIA, na.rm = TRUE)

## -----------------------------------------------------------------------------
twophase_boot_design <- as_gen_boot_design(
  design = twophase_design,
  variance_estimator = list(
    "Phase 1" = "Stratified Multistage SRS",
    "Phase 2" = "Ultimate Cluster"
  )
)

## -----------------------------------------------------------------------------
# Impute missing values (if necessary)
twophase_sample <- twophase_sample |>
  mutate(
    TOTCIR = ifelse(
      is.na(TOTCIR),
      stats::weighted.mean(TOTCIR, na.rm = TRUE,
                           w = 1/SAMPLING_PROB),
      TOTCIR
    ),
    TOTSTAFF = ifelse(
      is.na(TOTSTAFF),
      stats::weighted.mean(TOTSTAFF, na.rm = TRUE,
                           w = 1/SAMPLING_PROB),
      TOTSTAFF
    )
  )

## ---- warning=FALSE-----------------------------------------------------------
# Describe the two-phase survey design
  twophase_design <- twophase(
    method = "full",
    data = twophase_sample,
    # Identify the subset of first-phase elements
    # which were selected into the second-phase sample
    subset = ~ SECOND_PHASE_SELECTION,
    # Describe clusters, probabilities, and population sizes
    # at each phase of sampling
    id = list(~ PSU_ID + SSU_ID,
              ~ 1),
    probs = list(~ PSU_SAMPLING_PROB + SSU_SAMPLING_PROB,
                 NULL),
    fpc = list(~ PSU_POP_SIZE + SSU_POP_SIZE,
               NULL)
  )

# Create replicate weights for the second-phase sample
# (meant to reflect variance of the entire two-phase design)
  twophase_boot_design <- as_gen_boot_design(
    design = twophase_design,
    variance_estimator = list(
      "Phase 1" = "Stratified Multistage SRS",
      "Phase 2" = "Ultimate Cluster"
    ),
    replicates = 1000,
    mse = TRUE
  )

## -----------------------------------------------------------------------------
# Extract a survey design object representing the first phase sample
  first_phase_design <- twophase_design$phase1$full

# Create replicate weights for the first-phase sample
  first_phase_gen_boot <- as_gen_boot_design(
    design = first_phase_design,
    variance_estimator = "Stratified Multistage SRS",
    replicates = 1000
  )
  
# Estimate first-phase totals and their sampling-covariance
  first_phase_estimates <- svytotal(
    x = ~ TOTCIR + TOTSTAFF,
    design = first_phase_gen_boot
  )
  
  first_phase_totals <- coef(first_phase_estimates)
  first_phase_vcov <- vcov(first_phase_estimates)
  
  print(first_phase_totals)
  print(first_phase_vcov)

## -----------------------------------------------------------------------------
calibrated_twophase_design <- calibrate_to_estimate(
  rep_design = twophase_boot_design,
  # Specify the variables in the data to use for calibration
  cal_formula = ~ TOTCIR + TOTSTAFF,
  # Supply the first-phase estimates and their variance
  estimate = first_phase_totals,
  vcov_estimate = first_phase_vcov,
)

## -----------------------------------------------------------------------------
# Display second-phase estimates for calibration variables
svytotal(
  x = ~ TOTCIR + TOTSTAFF,
  design = calibrated_twophase_design
)

# Display the original first-phase estimates (which are identical!)
print(first_phase_estimates)

## -----------------------------------------------------------------------------
# Inspect calibrated second-phase estimate
svytotal(
  x = ~ LIBRARIA, na.rm = TRUE,
  design = calibrated_twophase_design
)

# Compare to uncalibrated second-phase estimate
svytotal(
  x = ~ LIBRARIA, na.rm = TRUE,
  design = twophase_boot_design
)

# Compare to first-phase estimate
svytotal(
  x = ~ LIBRARIA, na.rm = TRUE,
  design = first_phase_gen_boot
)

## -----------------------------------------------------------------------------
# Extract a survey design object representing the first phase sample
  first_phase_design <- twophase_design$phase1$full

# Create replicate weights for the first-phase sample
  first_phase_gen_boot <- as_gen_boot_design(
    design = first_phase_design,
    variance_estimator = "Stratified Multistage SRS",
    replicates = 1000
  )

## -----------------------------------------------------------------------------
calibrated_twophase_design <- calibrate_to_sample(
  primary_rep_design = twophase_boot_design,
  # Supply the first-phase replicate design
  control_rep_design = first_phase_gen_boot,
  # Specify the variables in the data to use for calibration
  cal_formula = ~ TOTCIR + TOTSTAFF
)

## -----------------------------------------------------------------------------
# Display second-phase estimates for calibration variables
calibrated_ests <- svytotal(
  x = ~ TOTCIR + TOTSTAFF,
  design = calibrated_twophase_design
)

print(calibrated_ests)

# Display the original first-phase estimates (which are identical!)
first_phase_ests <- svytotal(
  x = ~ TOTCIR + TOTSTAFF,
  design = first_phase_gen_boot
)

print(first_phase_ests)

## -----------------------------------------------------------------------------
ratio_of_variances <- vcov(calibrated_ests)/vcov(first_phase_ests)
ratio_of_variances

## -----------------------------------------------------------------------------
# Inspect calibrated second-phase estimate
svytotal(
  x = ~ LIBRARIA, na.rm = TRUE,
  design = calibrated_twophase_design
)

# Compare to uncalibrated second-phase estimate
svytotal(
  x = ~ LIBRARIA, na.rm = TRUE,
  design = twophase_boot_design
)

# Compare to first-phase estimate
svytotal(
  x = ~ LIBRARIA, na.rm = TRUE,
  design = first_phase_gen_boot
)

## -----------------------------------------------------------------------------
ratio_calib_design <- calibrate_to_sample(
  primary_rep_design = twophase_boot_design,
  # Supply the first-phase replicate design
  control_rep_design = first_phase_gen_boot,
  # Specify the GREG formula.
  # For ratio estimation, we add `-1` to the formula 
  # (i.e., we remove the intercept from the working model)
  # and specify only a single variable
  cal_formula = ~ -1 + TOTSTAFF,
  variance = 1
)

## -----------------------------------------------------------------------------
ratio_adjusted_weights <- weights(ratio_calib_design, type = "sampling")
unadjusted_weights <- weights(twophase_boot_design, type = "sampling")

adjustment_factors <- ratio_adjusted_weights/unadjusted_weights
head(adjustment_factors)

## -----------------------------------------------------------------------------
phase1_total <- svytotal(
  x = ~ TOTSTAFF,
  first_phase_design
) |> coef()
phase2_total <- svytotal(
  x = ~ TOTSTAFF,
  twophase_boot_design
) |> coef()

phase1_total/phase2_total

## -----------------------------------------------------------------------------
set.seed(2022)
y <- rnorm(n = 100)

# Select first phase sample, SRS without replacement
  phase_1_sample_indicators <- sampling::srswor(n = 50, N = 100) |>
    as.logical()
  
  phase_1_sample <- y[phase_1_sample_indicators]
  
# Make variance estimator for first-phase variance component
  Sigma_a <- make_quad_form_matrix(
    variance_estimator = "Ultimate Cluster",
    cluster_ids = as.matrix(1:50),
    strata_ids = rep(1, times = 50) |> as.matrix(),
    strata_pop_sizes = rep(100, times = 50) |> as.matrix()
  )

# Select second stage sample, SRS without replacment
  phase_2_sample_indicators <- sampling::srswor(n = 5, N = 50) |>
    as.logical()
  
  phase_2_sample <- phase_1_sample[phase_2_sample_indicators]
  
# Estimate two-phase variance
  Sigma_a_prime <- Sigma_a[phase_2_sample_indicators,
                           phase_2_sample_indicators]
  
  phase_2_joint_probs <- outer(rep(5/50, times = 5),
                               rep(4/49, times = 5))
  diag(phase_2_joint_probs) <- rep(5/50, times = 5)

  Sigma_b <- make_quad_form_matrix(
    variance_estimator = "Ultimate Cluster",
    cluster_ids = as.matrix(1:5),
    strata_ids = rep(1, times = 5) |> as.matrix(),
    strata_pop_sizes = rep(50, times = 5) |> as.matrix()
  )
  
  sigma_ab <- make_twophase_quad_form(
    sigma_1 = Sigma_a_prime,
    sigma_2 = Sigma_b,
    phase_2_joint_probs = phase_2_joint_probs
  )
  
  wts <- rep(
    (50/100)^(-1) * (5/50)^(-1),
    times = 5
  )
  W_star <- diag(wts)
  
  W_star_y <- W_star %*% phase_2_sample
  t(W_star_y) %*% sigma_ab %*% (W_star_y)
  
# Since both phases are SRS without replacement,
# variance estimate for a total should be similar to the following
  5 * var(W_star_y)


## -----------------------------------------------------------------------------
# Print first phase estimates and their variance-covariance
print(first_phase_totals)
print(first_phase_vcov)

# Calibrate the two-phase replicate design
# to the totals estimated from the first-phase sample
calibrated_twophase_design <- calibrate_to_estimate(
  rep_design = twophase_boot_design,
  # Specify the variables in the data to use for calibration
  cal_formula = ~ TOTCIR + TOTSTAFF,
  # Supply the first-phase estimates and their variance
  estimate = first_phase_totals,
  vcov_estimate = first_phase_vcov,
)

## -----------------------------------------------------------------------------
calibrated_twophase_design <- calibrate_to_sample(
  primary_rep_design = twophase_boot_design,
  # Supply the first-phase replicate design
  control_rep_design = first_phase_gen_boot,
  # Specify the variables in the data to use for calibration
  cal_formula = ~ TOTCIR + TOTSTAFF
)

## -----------------------------------------------------------------------------
gen_boot_design <- as_gen_boot_design(
  design = twophase_design,
  variance_estimator = list(
    'Phase 1' = "Ultimate Cluster",
    'Phase 2' = "Ultimate Cluster"
  )
)

## -----------------------------------------------------------------------------
twophase_quad_form_matrix <- get_design_quad_form(
  design = twophase_design,
  variance_estimator = list(
    'Phase 1' = "Ultimate Cluster",
    'Phase 2' = "Ultimate Cluster"
  )
)

twophase_quad_form_matrix |> is_psd_matrix()

## -----------------------------------------------------------------------------
approx_quad_form <- get_nearest_psd_matrix(twophase_quad_form_matrix)

## -----------------------------------------------------------------------------
# Extract weights and a single variable from the second-phase sample
## NOTE: To get second-phase data,
##       we use `my_design$phase1$sample$variables`.
##       To get first-phase data,
##       we use `my_design$phase1$full$variables

wts <- weights(twophase_design, type = "sampling")
y <- twophase_design$phase1$sample$variables$TOTSTAFF
wtd_y <- as.matrix(wts * y)

# Estimate standard errors
std_error <- as.numeric(
  t(wtd_y) %*% twophase_quad_form_matrix %*% wtd_y
) |> sqrt()

approx_std_error <- as.numeric(
  t(wtd_y) %*% approx_quad_form %*% wtd_y
) |> sqrt()

print(approx_std_error)
print(std_error)

approx_std_error / std_error

