# Correct sample density values using gradient position information

Used in conjunction with
[`plot_density_outliers()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_density_outliers.md),
this function will correct the gradient_pos_density values for outliers
based on the gradient_position values. A linear model is built using the
gradient_position and gradient_pos_density, and if the Cook's outlier
value is above the cutoff, then that sample's gradient_pos_density value
is replaced with the fitted value. If it isn't above the cut-off, then
no correction is made for that sample.

## Usage

``` r
correct_gradient_pos_density(sample_data, sensitivity = 4)
```

## Arguments

- sample_data:

  (*qsip_sample_data*) A qsip object with sample data

- sensitivity:

  (*numeric, default: 4*) A sensitivity value, with lower values being
  more sensitive to outlier detection and correction

## Value

A qsip_sample_data object with corrected gradient_pos_density values
