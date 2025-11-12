# Plot qSIP sample data density curves

Plot qSIP sample data density curves

## Usage

``` r
plot_sample_curves(
  qsip_data_object,
  title = NULL,
  facet_by = "source",
  show_wad = FALSE,
  colors = lifecycle::deprecated()
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) qSIP object

- title:

  (*character*) An optional title for the plot

- facet_by:

  (*character*) Facet the plots by "source" or by "isotope"

- show_wad:

  (*logical*) A logical value

- colors:

  deprecated

## Value

A ggplot object

## See also

Other "visualizations":
[`plot_density_outliers()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_density_outliers.md),
[`plot_feature_curves()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_feature_curves.md),
[`plot_filter_results()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_filter_results.md),
[`plot_source_wads()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_source_wads.md)
