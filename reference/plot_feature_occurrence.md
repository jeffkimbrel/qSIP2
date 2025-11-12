# Plot occurrence of features in samples

This is a plotting function to visualize the occurrence of features in
samples. The function takes a qsip_data object and a vector of
feature_ids, and can scale the results by total abundance or source
abundance, and the WAD value can also be shown.

## Usage

``` r
plot_feature_occurrence(
  qsip_data_object,
  feature_ids = NULL,
  scale = "none",
  show_wad = FALSE,
  title = NULL,
  legend.position = "right"
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip_data object

- feature_ids:

  (*character*) An optional vector of feature_ids

- scale:

  (*character*) A character string

- show_wad:

  (*logical*) A logical value

- title:

  (*character*) A character string

- legend.position:

  (*character* or *numeric vector*) Values passed to
  ggplot2::theme(legend.position = ...)

## Value

Returns a ggplot object
