# Plot the resampled EAFs for each feature

This plot will show the results of the resampling procedure for each
feature. The resampling procedure is run using the run_resampling()
function. The plot will show the mean resampled EAF for each feature,
with the confidence interval (default 90%) shown as a bar or line
(default no line). The area under the curve can also be shown (default
TRUE).

## Usage

``` r
plot_feature_resamplings(
  qsip_data_object,
  feature_ids = NULL,
  confidence = 0.9,
  intervals = "",
  points = FALSE
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object that has been resampled

- feature_ids:

  (*character vector*) A vector of feature ids to filter on

- confidence:

  (*numeric*) The confidence interval to plot

- intervals:

  (*character*) Whether to plot the confidence interval as a bar, line
  or not at all (default)

- points:

  (*boolean*) Whether to show the points or not

## Value

A ggplot object
