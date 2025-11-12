# Correct gradient position density (gpd) values using linear regression approach

Correct gradient position density (gpd) values using linear regression
approach

## Usage

``` r
correct_gpd_rlm(
  qsip_data_object,
  method = "siegel",
  fraction_cutoff = 5,
  source_cutoff = 3,
  quiet = F,
  return = "qsip_data_object"
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object

- method:

  (*character*) The lm method to use

- fraction_cutoff:

  (*numeric, default: 5*) The minimum number of fractions a feature must
  be found in to be considered

- source_cutoff:

  (*numeric, default: 3*) The minimum number of sources a feature must
  be found in to be considered

- quiet:

  (*logical, default: FALSE*) Whether to print a message about the
  number of features found

- return:

  (\*character, default: "qsip_data_object") Whether to return the
  corrections or the qsip_data_object
