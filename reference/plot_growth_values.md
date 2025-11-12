# Plot growth values

Plot growth values

## Usage

``` r
plot_growth_values(
  qsip_data_object,
  confidence = 0.9,
  top = Inf,
  error = "none",
  alpha = 0.4,
  type = "rates"
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object

- confidence:

  (*numeric*) The confidence level for the confidence interval

- top:

  (*numeric*) The number of top features to plot. Use `Inf` for all

- error:

  (*character*) The type of error bars to plot. Options are 'none',
  'bar', 'ribbon'

- alpha:

  (*numeric*) The transparency of the error bar/ribbon

- type:

  (*character*) The type of growth values to plot. Options are 'rates'
  or "copies
