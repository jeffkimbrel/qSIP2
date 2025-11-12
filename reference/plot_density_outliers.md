# Cook's outlier detection on gradient positions vs densities

Assuming a linear relationship between the gradient_position and
gradient_pos_density, this function will plot any potential outliers
using Cook's distance and a defined sensitivity. The lower the
sensitivity, the more likely outliers will be flagged.

## Usage

``` r
plot_density_outliers(sample_data, sensitivity = 4)
```

## Arguments

- sample_data:

  (*qsip_sample_data or qsip_data*) A qsip object with sample data

- sensitivity:

  (*numeric, default: 4*) A sensitivity value, with lower values being
  more sensitive to outlier detection

## Value

A ggplot object

## See also

Other "visualizations":
[`plot_feature_curves()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_feature_curves.md),
[`plot_filter_results()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_filter_results.md),
[`plot_sample_curves()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_sample_curves.md),
[`plot_source_wads()`](https://jeffkimbrel.github.io/qSIP2/reference/plot_source_wads.md)
