
<!-- README.md is generated from README.Rmd. Please edit that file -->

# svrep

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bschneidr/svrep/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bschneidr/svrep?branch=main)
<!-- [![R-CMD-check](https://github.com/bschneidr/svrep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bschneidr/svrep/actions/workflows/R-CMD-check.yaml) -->
<!-- badges: end -->

svrep provides methods for creating, updating, and analyzing replicate
weights for surveys. Functions from svrep can be used to implement
adjustments to replicate designs (e.g.¬†nonresponse weighting class
adjustments) and analyze their effect on the replicate weights and on
estimates of interest.

## Installation

You can install the released version of svrep from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("svrep")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bschneidr/svrep")
```

## Example usage

Suppose we have a replicate-weights survey design object created with
the survey package. This survey design object can include respondents,
non-respondents, and cases with unknown eligibility.

``` r
library(survey)
library(svrep)
data(api, package = "survey")
set.seed(2021)

# Create variable giving response status
apiclus1$response_status <- sample(x = c("Respondent", "Nonrespondent",
                                         "Ineligible", "Unknown eligibility"),
                                   size = nrow(apiclus1),
                                   replace = TRUE)

# Create replicate-weights survey design
dclus1 <- svydesign(data = apiclus1,
                    id = ~dnum, weights = ~pw, fpc = ~fpc)

orig_rep_design <- as.svrepdesign(dclus1)

print(orig_rep_design)
#> Call: as.svrepdesign.default(dclus1)
#> Unstratified cluster jacknife (JK1) with 15 replicates.
```

### Adjusting for non-response or unknown eligibility

It is common practice to adjust weights when there is non-response or
there are sampled cases whose eligibility for the survey is unknown. The
most common form of adjustment is ‚Äúweight redistribution‚ÄĚ: for example,
weights from non-respondents are reduced to zero, and weights from
respondents are correspondingly increased so that the total weight in
the sample is unchanged. In order to account for these adjustments when
estimating variances for survey statistics, the adjustments are repeated
separately for each set of replicate weights. This process can be easily
implemented using the `redistribute_weights()` function.

``` r
# Adjust weights for unknown eligibility
ue_adjusted_design <- redistribute_weights(design = orig_rep_design,
                                           reduce_if = response_status %in% c("Unknown eligibility"),
                                           increase_if = !response_status %in% c("Unknown eligibility"))
```

By supplying column names to the `by` argument of
`redistribute_weights()`, adjustments are conducted separately in
different groups. This can be used to conduct nonresponse weighting
class adjustments.

``` r
nr_adjusted_design <- redistribute_weights(design = ue_adjusted_design,
                                           reduce_if = response_status == "Nonrespondent",
                                           increase_if = response_status == "Respondent",
                                           by = c("stype"))
```

### Comparing estimates from different sets of weights

In order to assess whether weighting adjustments have an impact on the
estimates we care about, we want to compare the estimates from the
different sets of weights. The function `svyby_repwts()` makes it easy
to compare estimates from different sets of weights.

``` r
# Estimate overall means (and their standard errors) from each design
overall_estimates <- svyby_repwts(
  rep_designs = list('original' = orig_rep_design,
                     'nonresponse-adjusted' = nr_adjusted_design),
  formula = ~ api00, FUN = svymean
)
print(overall_estimates, row.names = FALSE)
#>           Design_Name    api00       se
#>  nonresponse-adjusted 646.4465 30.66081
#>              original 644.1694 26.32936

# Estimate domain means (and their standard errors) from each design
domain_estimates <- svyby_repwts(
  rep_designs = list('original' = orig_rep_design,
                     'nonresponse-adjusted' = nr_adjusted_design),
  formula = ~ api00, by = ~ stype, FUN = svymean
)
print(domain_estimates, row.names = FALSE)
#>           Design_Name stype    api00       se
#>  nonresponse-adjusted     E 641.9463 34.19443
#>              original     E 648.8681 25.37430
#>  nonresponse-adjusted     H 699.5455 14.24657
#>              original     H 618.5714 46.34412
#>  nonresponse-adjusted     M 643.3429 41.47212
#>              original     M 631.4400 33.68762
```

We can even test for differences in estimates from the two sets of
weights and calculate confidence intervals for their difference.

``` r
estimates <- svyby_repwts(
  rep_designs = list('original' = orig_rep_design,
                     'nonresponse-adjusted' = nr_adjusted_design),
  formula = ~ api00, FUN = svymean
)

vcov(estimates)
#>                      nonresponse-adjusted original
#> nonresponse-adjusted             940.0856 784.1324
#> original                         784.1324 693.2352

diff_between_ests <- svycontrast(stat = estimates,
                                 contrasts = list(
                                   "Original vs. Adjusted" = c(-1,1)
                                 ))
print(diff_between_ests)
#>                       contrast     SE
#> Original vs. Adjusted  -2.2771 8.0657
confint(diff_between_ests)
#>                           2.5 %   97.5 %
#> Original vs. Adjusted -18.08562 13.53147
```

### Diagnosing potential issues with weights

When adjusting replicate weights, there are several diagnostics which
can be used to ensure that the adjustments were carried out correctly
and that they do more good than harm. The function
`summarize_rep_weights()` helps by allowing you to quickly summarize the
replicate weights.

For example, when carrying out nonresponse adjustments, we might want to
verify that all of the weights for nonrespondents have been set to zero
in each replicate. We can use the `summarize_rep_weights()` to compare
summary statistics for each replicate, and we can use its `by` argument
to group the summaries by one or more variables.

``` r
summarize_rep_weights(
  rep_design = nr_adjusted_design,
  type = 'specific',
  by = "response_status"
) |> 
  subset(Rep_Column %in% 1:2)
#>        response_status Rep_Column  N N_NONZERO      SUM     MEAN        CV
#> 1           Ineligible          1 50        47 2109.089 42.18178 0.2552106
#> 2           Ineligible          2 50        49 2224.316 44.48631 0.1443075
#> 16       Nonrespondent          1 48         0    0.000  0.00000       NaN
#> 17       Nonrespondent          2 48         0    0.000  0.00000       NaN
#> 31          Respondent          1 49        49 4128.429 84.25366 0.2403636
#> 32          Respondent          2 49        48 4267.055 87.08275 0.2224368
#> 46 Unknown eligibility          1 36         0    0.000  0.00000       NaN
#> 47 Unknown eligibility          2 36         0    0.000  0.00000       NaN
#>         MIN       MAX
#> 1   0.00000  44.87423
#> 2   0.00000  45.39420
#> 16  0.00000   0.00000
#> 17  0.00000   0.00000
#> 31 70.51665 179.49692
#> 32  0.00000 158.87969
#> 46  0.00000   0.00000
#> 47  0.00000   0.00000
```

At the end of the adjustment process, we can inspect the number of rows
and columns and examine the variability of the weights across all of the
replicates.

``` r
nr_adjusted_design |>
  subset(response_status == "Respondent") |>
  summarize_rep_weights(
    type = 'overall'
  )
#>   nrows ncols degf_svy_pkg rank avg_wgt_sum sd_wgt_sums min_rep_wgt max_rep_wgt
#> 1    49    15           14   15    4087.158    259.0107           0    316.8524
```

### Sample-based calibration

When we rake or poststratify to estimated control totals rather than to
‚Äútrue‚ÄĚ population values, we may need to account for the variance of the
estimated control totals to ensure that calibrated estimates
appropriately reflect sampling error of both the primary survey of
interest and the survey from which the control totals were estimated.
The ‚Äėsvrep‚Äô package provides two functions which accomplish this. The
function `calibrate_to_estimate()` requires the user to supply a vector
of control totals and its variance-covariance matrix, while the function
`calibrate_to_sample()` requires the user to supply a dataset with
replicate weights to use for estimating control totals and their
sampling variance.

As an example, suppose we have a survey measuring vaccination status of
adults in Louisville, Kentucky. For variance estimation, we use 100
bootstrap replicates.

``` r
data("lou_vax_survey")

# Load example data
lou_vax_survey <- svydesign(ids = ~ 1, weights = ~ SAMPLING_WEIGHT,
                            data = lou_vax_survey) |>
  as.svrepdesign(type = "boot", replicates = 100, mse = TRUE)

# Adjust for nonresponse
lou_vax_survey <- lou_vax_survey |>
  redistribute_weights(
    reduce_if = RESPONSE_STATUS == "Nonrespondent",
    increase_if = RESPONSE_STATUS == "Respondent"
  ) |>
  subset(RESPONSE_STATUS == "Respondent")
```

To reduce nonresponse bias or coverage error for the survey, we can rake
the survey to population totals for demographic groups estimated by the
Census Bureau in the American Community Survey (ACS). To estimate the
population totals for raking purposes, we can use microdata with
replicate weights.

``` r
# Load microdata to use for estimating control totals
data("lou_pums_microdata")

acs_benchmark_survey <- survey::svrepdesign(
  data = lou_pums_microdata,
  variables = ~ UNIQUE_ID + AGE + SEX + RACE_ETHNICITY + EDUC_ATTAINMENT,
  weights = ~ PWGTP, repweights = "PWGTP\\d{1,2}",
  type = "successive-difference",
  mse = TRUE
)
```

We can see that the distribution of race/ethnicity among respondents
differs from the distribution of race/ethnicity in the ACS benchmarks.

``` r
# Compare demographic estimates from the two data sources
estimate_comparisons <- data.frame(
  'Vax_Survey' = svymean(x = ~ RACE_ETHNICITY, design = acs_benchmark_survey) |> coef(),
  'ACS_Benchmark' = svymean(x = ~ RACE_ETHNICITY, design = lou_vax_survey) |> coef()
)
rownames(estimate_comparisons) <- gsub(x = rownames(estimate_comparisons),
                                       "RACE_ETHNICITY", "")
print(estimate_comparisons)
#>                                                         Vax_Survey
#> Black or African American alone, not Hispanic or Latino 0.19949824
#> Hispanic or Latino                                      0.04525039
#> Other Race, not Hispanic or Latino                      0.04630955
#> White alone, not Hispanic or Latino                     0.70894182
#>                                                         ACS_Benchmark
#> Black or African American alone, not Hispanic or Latino    0.16932271
#> Hispanic or Latino                                         0.03386454
#> Other Race, not Hispanic or Latino                         0.05776892
#> White alone, not Hispanic or Latino                        0.73904382
```

There are two options for calibrating the sample to the control totals
from the benchmark survey. With the first approach, we supply point
estimates and their variance-covariance matrix to the function
`calibrate_to_estimate()`.

``` r
# Estimate control totals and their variance-covariance matrix
control_totals <- svymean(x = ~ RACE_ETHNICITY + EDUC_ATTAINMENT,
                          design = acs_benchmark_survey)
point_estimates <- coef(control_totals)
vcov_estimates <- vcov(control_totals)

# Calibrate the vaccination survey to the estimated control totals
vax_survey_raked_to_estimates <- calibrate_to_estimate(
  rep_design = lou_vax_survey,
  estimate = point_estimates,
  vcov_estimate = vcov_estimates,
  cal_formula = ~ RACE_ETHNICITY + EDUC_ATTAINMENT,
  calfun = survey::cal.raking
)
```

With the second approach, we supply the control survey‚Äôs replicate
design to `calibrate_to_sample()`.

``` r
vax_survey_raked_to_acs_sample <- calibrate_to_sample(
  primary_rep_design = lou_vax_survey,
  control_rep_design = acs_benchmark_survey,
  cal_formula = ~ RACE_ETHNICITY + EDUC_ATTAINMENT,
  calfun = survey::cal.raking
)
```

After calibration, we can see that the estimated vaccination rate has
decreased, and the estimated standard error of the estimated vaccination
rate has increased.

``` r
# Compare the two sets of estimates
svyby_repwts(
  rep_design = list(
    'NR-adjusted' = lou_vax_survey,
    'Raked to estimate' = vax_survey_raked_to_estimates,
    'Raked to sample' = vax_survey_raked_to_acs_sample
  ),
  formula = ~ VAX_STATUS,
  FUN = svymean,
  keep.names = FALSE
)
#>         Design_Name VAX_STATUSUnvaccinated VAX_STATUSVaccinated        se1
#> 1       NR-adjusted              0.4621514            0.5378486 0.02430176
#> 2 Raked to estimate              0.4732623            0.5267377 0.02448676
#> 3   Raked to sample              0.4732623            0.5267377 0.02446881
#>          se2
#> 1 0.02430176
#> 2 0.02448676
#> 3 0.02446881
```

### Saving results to a data file

Once we‚Äôre satisfied with the weights, we can create a data frame with
the analysis variables and columns of final full-sample weights and
replicate weights. This format is easy to export to data files that can
be loaded into R or other software later.

``` r
data_frame_with_final_weights <- vax_survey_raked_to_estimates |>
  as_data_frame_with_weights(
    full_wgt_name = "RAKED_WGT",
    rep_wgt_prefix = "RAKED_REP_WGT_"
  )

# Preview first 10 column names
colnames(data_frame_with_final_weights) |> head(10)
#>  [1] "RESPONSE_STATUS" "RACE_ETHNICITY"  "SEX"             "EDUC_ATTAINMENT"
#>  [5] "VAX_STATUS"      "SAMPLING_WEIGHT" "RAKED_WGT"       "RAKED_REP_WGT_1"
#>  [9] "RAKED_REP_WGT_2" "RAKED_REP_WGT_3"
```

``` r
# Write the data to a CSV file
write.csv(
  x = data_frame_with_final_weights,
  file = "survey-data_with-updated-weights.csv"
)
```
