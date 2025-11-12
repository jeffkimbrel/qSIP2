# Correct gradient position density (gpd) values using bootstrapped features

Correct gradient position density (gpd) values using bootstrapped
features

## Usage

``` r
correct_gpd_bootstrap(
  qsip_data_object,
  bootstraps = 1000,
  fraction_cutoff = 5,
  source_cutoff = 3,
  quiet = F,
  return = "qsip_data_object"
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object

- bootstraps:

  (*numeric, default: 1000*) The number of bootstraps to perform

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
