# Plot EAF and confidence intervals

This function plots the observed EAF values for each feature in the
dataset. The features are ordered by their observed EAF values. The
confidence intervals are plotted as error bars or ribbons.

## Usage

``` r
plot_EAF_values(
  qsip_data_object,
  confidence = 0.9,
  success_ratio = 0.9,
  top = Inf,
  error = "none",
  alpha = 0.3,
  zero_line = TRUE,
  shared_y = FALSE,
  title = NULL,
  taxonomy = NULL
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip_data object or list of qsip_data objects

- confidence:

  (*numeric*) The confidence level for the confidence interval

- success_ratio:

  (*numeric*) The ratio of successful resamples to total resamples

- top:

  (*numeric*) The number of top features to plot. Use `Inf` for all

- error:

  (*character*) The type of error bars to plot. Options are 'none',
  'bar', 'ribbon'

- alpha:

  (*numeric*) The transparency of the error bar/ribbon

- zero_line:

  (*logical*) Add a line at EAF = 0

- shared_y:

  (*logical*) Use a shared y-axis for the facets

- title:

  (*character*) An optional title of the plot

- taxonomy:

  (*logical*) If TRUE, the taxonomy will be added to the plot

## Details

Either a single qsip object or a list of named qsip objects can be
passed to this function. If giving a list of qsip objects the plot will
be faceted by the list names.

If the resampling step was run with the default
`allow_failures = FALSE`, then the points will just be colored a generic
blue. But, if resampling was instead run with `allow_failures = TRUE`,
then the points are colored based on the success ratio of the resamples.
If giving a list of qsip objects then the pass/fail color scheme will be
applied if any of the qsip objects have `allow_failures = TRUE`.
