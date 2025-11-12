# Plot difference between feature WAD and WAD reference

Plot difference between feature WAD and WAD reference

## Usage

``` r
plot_difference_to_mean(
  qsip_data_object,
  fraction_cutoff = 5,
  source_cutoff = 3,
  quiet = F
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object

- fraction_cutoff:

  (*numeric, default: 5*) The minimum number of fractions a feature must
  be found in to be considered

- source_cutoff:

  (*numeric, default: 3*) The minimum number of sources a feature must
  be found in to be considered

- quiet:

  (*logical, default: FALSE*) Whether to print a message about the
  number of features found
