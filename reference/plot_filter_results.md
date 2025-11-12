# Plot the results of filtering features

After running the
[`run_feature_filter()`](https://jeffkimbrel.github.io/qSIP2/reference/run_feature_filter.md)
code, this function will produce two plot detailing the consequences of
the filtering. Plot A shows the retained and removed features by their
total tube relative abundance contribution, and plot B shows the
retained and removed features by the total count of each category. The
"zero fractions" shown in plot B are those entirely missing from the
given source_mat_id and are thus not going to be retained no matter what
the `run_filter_feature()` parameters are.

## Usage

``` r
plot_filter_results(
  qsip_data_object,
  return_type = "combined",
  colors = lifecycle::deprecated()
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*)

- return_type:

  (*string, default: combined*) Changes the return type from a combined
  plot (*combined*), list of individual plots (*individual*) or list of
  dataframes (*dataframe*)

- colors:

  deprecated

## Value

Combined or individual plots, or the raw dataframes

## See also

Other "visualizations":
[`plot_density_outliers()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_density_outliers.md),
[`plot_feature_curves()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_feature_curves.md),
[`plot_sample_curves()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_sample_curves.md),
[`plot_source_wads()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_source_wads.md)
