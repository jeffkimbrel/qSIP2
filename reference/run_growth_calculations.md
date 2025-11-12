# Run growth calculations

Run growth calculations

## Usage

``` r
run_growth_calculations(
  qsip_data_object,
  N_total_it,
  growth_model = "exponential",
  timepoint = "timepoint",
  correct_copy_numbers = "filter",
  correct_EAF = "filter",
  propO = 1
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) An object of `qsip_data` class

- N_total_it:

  (*data.frame*) A data frame of time zero totals from
  [`get_N_total_it()`](https://jeffkimbrel.github.io/qSIP2/reference/get_N_total_it.md)

- growth_model:

  (*character, default: exponential*) The growth model to use. Must be
  either "exponential" or "linear"

- timepoint:

  (*character*) The name of the timepoint column in the source data

- correct_copy_numbers:

  (*character, default: filter*) If copy numbers are not logical (e.g.
  \< 0), should they be filtered out or adjusted to 0?

- correct_EAF:

  (*character, default: filter*) If EAF values are not logical (e.g. \<0
  or \>1), should they be filtered out or adjusted to 0 or 1?

- propO:

  (*numeric*) The proportion of heavy isotope in the labeled DNA. Only
  used for 18O.
