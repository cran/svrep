## ----include = FALSE----------------------------------------------------------
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
library(survey) # For complex survey analysis
library(svrep)

set.seed(2022)

# Load an example dataset from a multistage sample, with two stages of SRSWOR
  data("mu284", package = 'survey')
  multistage_srswor_design <- svydesign(data = mu284,
                                        ids = ~ id1 + id2,
                                        fpc = ~ n1 + n2)

  bootstrap_rep_design <- as_bootstrap_design(multistage_srswor_design,
                                              type = "Rao-Wu-Yue-Beaumont",
                                              replicates = 500)
  
  svytotal(x = ~ y1, design = multistage_srswor_design)
  svytotal(x = ~ y1, design = bootstrap_rep_design)

## ----eval=FALSE---------------------------------------------------------------
#  # Load example dataset of U.S. counties and states with 2004 Presidential vote counts
#    data("election", package = 'survey')
#    pps_wor_design <- svydesign(data = election_pps,
#                                pps = HR(),
#                                fpc = ~ p, # Inclusion probabilities
#                                ids = ~ 1)
#    bootstrap_rep_design <- as_bootstrap_design(pps_wor_design,
#                                                type = "Rao-Wu-Yue-Beaumont",
#                                                replicates = 100)
#  
#    svytotal(x = ~ Bush + Kerry, design = pps_wor_design)
#    svytotal(x = ~ Bush + Kerry, design = bootstrap_rep_design)

## -----------------------------------------------------------------------------
# Declare a multistage design
# where first-stage probabilities are PPSWOR sampling
# and second-stage probabilities are based on SRSWOR
multistage_design <- svydesign(
  data = library_multistage_sample,
  ids = ~ PSU_ID + SSU_ID,
  probs = ~ PSU_SAMPLING_PROB + SSU_SAMPLING_PROB,
  pps = "brewer"
)

# Convert to a bootstrap replicate design
boot_design <- as_bootstrap_design(
  design = multistage_design,
  type = "Rao-Wu-Yue-Beaumont",
  samp_method_by_stage = c("PPSWOR", "SRSWOR"),
  replicates = 1000
)

# Compare variance estimates
svytotal(x = ~ TOTCIR, na.rm = TRUE, design = multistage_design)
svytotal(x = ~ TOTCIR, na.rm = TRUE, design = boot_design)

## -----------------------------------------------------------------------------
make_quad_form_matrix(
    variance_estimator = "SD2",
    cluster_ids = c(1,2,3,4,5) |> data.frame(),
    strata_ids = c(1,1,1,1,1) |> data.frame(),
    sort_order = c(1,2,3,4,5)
)

## ----eval=TRUE----------------------------------------------------------------
# Load an example dataset of a stratified systematic sample
data('library_stsys_sample', package = 'svrep')

# First, sort the rows in the order used in sampling
library_stsys_sample <- library_stsys_sample |>
  dplyr::arrange(SAMPLING_SORT_ORDER)

# Create a survey design object
survey_design <- svydesign(
  data = library_stsys_sample,
  ids = ~ 1,
  strata = ~ SAMPLING_STRATUM,
  fpc = ~ STRATUM_POP_SIZE
)

# Obtain the quadratic form for the target estimator
sd2_quad_form <- get_design_quad_form(
  design = survey_design,
  variance_estimator = "SD2"
)

class(sd2_quad_form)
dim(sd2_quad_form)

## -----------------------------------------------------------------------------
# Obtain weighted values
wtd_y <- as.matrix(library_stsys_sample[['LIBRARIA']] /
                     library_stsys_sample[['SAMPLING_PROB']])
wtd_y[is.na(wtd_y)] <- 0

# Obtain point estimate for a population total
point_estimate <- sum(wtd_y)

# Obtain the variance estimate using the quadratic form
variance_estimate <- t(wtd_y) %*% sd2_quad_form %*% wtd_y
std_error <- sqrt(variance_estimate[1,1])

# Summarize results
sprintf("Estimate: %s", round(point_estimate))
sprintf("Standard Error: %s", round(std_error))

## -----------------------------------------------------------------------------
# Load example data from stratified systematic sample
data('library_stsys_sample', package = 'svrep')

# First, ensure data are sorted in same order as was used in sampling
library_stsys_sample <- library_stsys_sample[
  order(library_stsys_sample$SAMPLING_SORT_ORDER),
]

# Create a survey design object
design_obj <- svydesign(
  data = library_stsys_sample,
  strata = ~ SAMPLING_STRATUM,
  ids = ~ 1,
  fpc = ~ STRATUM_POP_SIZE
)

## -----------------------------------------------------------------------------
# Convert to generalized bootstrap replicate design
gen_boot_design_sd2 <- as_gen_boot_design(
  design = design_obj,
  variance_estimator = "SD2",
  replicates = 2000
)

# Estimate sampling variances
svymean(x = ~ TOTSTAFF, na.rm = TRUE, design = gen_boot_design_sd2)

## ----eval=FALSE---------------------------------------------------------------
#  # Load example data of a PPS survey of counties and states
#     data('election', package = 'survey')
#  
#  # Create survey design object
#     pps_design_ht <- svydesign(
#       data = election_pps,
#       id = ~1, fpc = ~p,
#       pps = ppsmat(election_jointprob),
#       variance = "HT"
#     )
#  
#  
#  # Convert to generalized bootstrap replicate design
#  gen_boot_design_ht <- pps_design_ht |>
#    as_gen_boot_design(variance_estimator = "Horvitz-Thompson",
#                       replicates = 5000, tau = "auto")
#  
#  # Compare sampling variances from bootstrap vs. Horvitz-Thompson estimator
#  svytotal(x = ~ Bush + Kerry, design = pps_design_ht)
#  svytotal(x = ~ Bush + Kerry, design = gen_boot_design_ht)

## -----------------------------------------------------------------------------
library(dplyr) # For data manipulation

# Create a multistage survey design
  multistage_design <- svydesign(
    data = library_multistage_sample |>
      mutate(Weight = 1/SAMPLING_PROB),
    ids = ~ PSU_ID + SSU_ID,
    fpc = ~ PSU_POP_SIZE + SSU_POP_SIZE,
    weights = ~ Weight
  )

# Convert to a generalized bootstrap design
  multistage_boot_design <- as_gen_boot_design(
    design = multistage_design,
    variance_estimator = "Stratified Multistage SRS"
  )

# Compare variance estimates
  svytotal(x = ~ TOTCIR, na.rm = TRUE, design = multistage_design)
  svytotal(x = ~ TOTCIR, na.rm = TRUE, design = multistage_boot_design)

## -----------------------------------------------------------------------------
# View overall scale factor
overall_scale_factor <- multistage_boot_design$scale
print(overall_scale_factor)

# Check that the scale factor was calculated correctly
tau <- multistage_boot_design$tau
print(tau)
B <- ncol(multistage_boot_design$repweights)
print(B)

print( (tau^2) / B )

## -----------------------------------------------------------------------------
# Load an example dataset of a stratified systematic sample
data('library_stsys_sample', package = 'svrep')

# Represent the SD2 successive-difference estimator as a quadratic form,
# and obtain the matrix of that quadratic form
sd2_quad_form <- make_quad_form_matrix(
  variance_estimator = 'SD2',
  cluster_ids = library_stsys_sample |> select(FSCSKEY),
  strata_ids = library_stsys_sample |> select(SAMPLING_STRATUM),
  strata_pop_sizes = library_stsys_sample |> select(STRATUM_POP_SIZE),
  sort_order = library_stsys_sample |> pull("SAMPLING_SORT_ORDER")
)

## -----------------------------------------------------------------------------
rep_adj_factors <- make_gen_boot_factors(
  Sigma = sd2_quad_form,
  num_replicates = 500,
  tau = "auto"
)

## -----------------------------------------------------------------------------
tau <- attr(rep_adj_factors, 'tau')
B <- ncol(rep_adj_factors)

## -----------------------------------------------------------------------------
# Retrieve value of 'scale'
rep_adj_factors |>
  attr('scale') 

# Compare to manually-calculated value
  (tau^2) / B

# Retrieve value of 'rscales'
rep_adj_factors |>
  attr('rscales') |> 
  head() # Only show first 5 values

## -----------------------------------------------------------------------------
gen_boot_design <- svrepdesign(
  data = library_stsys_sample |>
    mutate(SAMPLING_WEIGHT = 1/SAMPLING_PROB),
  repweights = rep_adj_factors,
  weights = ~ SAMPLING_WEIGHT,
  combined.weights = FALSE,
  type = "other",
  scale = attr(rep_adj_factors, 'scale'),
  rscales = attr(rep_adj_factors, 'rscales')
)

## -----------------------------------------------------------------------------
gen_boot_design |>
  svymean(x = ~ TOTSTAFF,
          na.rm = TRUE, deff = TRUE)

## -----------------------------------------------------------------------------
library(survey)
data('api', package = 'survey')

# Declare a bootstrap survey design object ----
  boot_design <- svydesign(
    data = apistrat,
    weights = ~pw,
    id = ~1,
    strata = ~stype,
    fpc = ~fpc
  ) |>
    svrep::as_bootstrap_design(replicates = 5000)

# Produce estimates of interest, and save the estimate from each replicate ----
  estimated_means_and_proportions <- svymean(x = ~ api00 + api99 + stype,
                                             design = boot_design,
                                             return.replicates = TRUE)

# Estimate the number of replicates needed to obtain a target simulation CV ----
  estimate_boot_reps_for_target_cv(
    svrepstat = estimated_means_and_proportions,
    target_cv = c(0.01, 0.05, 0.10)
  )

## -----------------------------------------------------------------------------
estimate_boot_sim_cv(estimated_means_and_proportions)

