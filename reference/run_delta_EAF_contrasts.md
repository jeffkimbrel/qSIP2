# Get delta EAF values between contrast groupings

Given a list of qSIP2 objects, this function will return the delta EAF
value for each feature_id, including the confidence interval, standard
deviation, and two p-values from 1) the bootstrap percentile test
(`bs_pval`) and 2) the Wald's z-test (`pval`).

## Usage

``` r
run_delta_EAF_contrasts(q, contrasts = NULL, confidence = 0.95, quiet = FALSE)
```

## Arguments

- q:

  a qSIP2 list object

- contrasts:

  A validated contrasts table

- confidence:

  (numeric, default: 0.95) confidence interval

## Value

a tibble

## Details

By default the function will do all pairwise comparisons of the groups
in the object list with the first listed as the "control" and the second
as the "treatment". Optionally, data frame with "control", "treatment"
and "contrast" columns can be provided to specify only the contrasts of
interest and to define the proper "control" vs. "treatment" groups. To
ensure this data frame is properly formatted (e.g. has the proper column
names and all groups are found in the qSIP2 object list), it must be
validated through the
[`validate_delta_EAF_contrasts()`](https://jeffkimbrel.github.io/qSIP2/reference/validate_delta_EAF_contrasts.md)
function.

Under the hood, for each feature_id this function subtracts the observed
EAF in the "treatment" group from the "control" and saves this as the
\\\delta\\ value. Then, it pulls the data from
[`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md)
and for each resampling \\i\\ calculates the delta (so \\\delta_i =
control_i - treatment_i\\) saving these values as a vector of delta
distributions. The `bs_pval` is calculated from this distribution, and
`pval` is calculated from \\\delta\\ and the standard deviation of the
distribution.
