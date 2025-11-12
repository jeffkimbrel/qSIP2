# Plot the correction statistics

Plot the correction statistics

## Usage

``` r
plot_correction_stats(
  reference,
  corrected,
  source_cutoff = 3,
  fraction_cutoff = 5,
  quiet = F
)
```

## Arguments

- reference:

  (*qsip_data*) A qsip data object

- corrected:

  (*qsip_data*) A qsip data object

- source_cutoff:

  (*numeric, default: 3*) The minimum number of sources a feature must
  be found in to be considered

- fraction_cutoff:

  (*numeric, default: 5*) The minimum number of fractions a feature must
  be found in to be considered

- quiet:

  (*logical, default: FALSE*) Whether to print a message about the
  number of features found
