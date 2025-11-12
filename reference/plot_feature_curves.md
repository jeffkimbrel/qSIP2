# Plot qSIP feature data density curves

Plot qSIP feature data density curves

## Usage

``` r
plot_feature_curves(
  qsip_data_object,
  feature_ids,
  source_mat_ids = NULL,
  scale = "source",
  color_by = "source",
  title = NULL
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object with tube relative abundances

- feature_ids:

  (*string*) Feature ids to be plotted on their own facet

- source_mat_ids:

  (*string, defaults to all*) A list of source material ids

- scale:

  (*string*) A string indicating how to scale the y-axis. Options are
  "feature" or "source"

- color_by:

  (*string*) A string indicating how to color the lines. Options are
  "source" or "isotope"

- title:

  (*string*) An optional title for the plot

## Value

A ggplot object

## See also

Other "visualizations":
[`plot_density_outliers()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_density_outliers.md),
[`plot_filter_results()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_filter_results.md),
[`plot_sample_curves()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_sample_curves.md),
[`plot_source_wads()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_source_wads.md)
