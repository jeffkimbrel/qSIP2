# Plot the number of successful resamples for each feature_id

This function will plot the number of successful resamples for each
feature_id. This value will typically be the number of resamples given
to
[`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md),
but if
[`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md)
is called with `allow_failures = TRUE` then the number of successful
resamples might be less than the number of resamples given.

## Usage

``` r
plot_successful_resamples(
  qsip_data_object,
  labels = FALSE,
  as_percentage = FALSE
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object that has been resampled

- labels:

  (*logical*) If TRUE, the counts will be labeled on the plot

- as_percentage:

  (*logical*) If TRUE, the x axis will be the percentage of the total
  number of resamples

## Value

A ggplot object
