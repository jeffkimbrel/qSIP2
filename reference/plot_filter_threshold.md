# Plot features with stacked bars showing zero/below threshold/passed

Shows the composition of filtering results:

- Dark colors: Missing/Zero (will never pass)

- Medium colors: Below Threshold (tunable with filtering parameters)

- Full colors: Passed threshold

## Usage

``` r
plot_filter_threshold(qsip_data_object, use_counts = FALSE)
```

## Arguments

- qsip_data_object:

  A filtered qsip_data object (or list)

## Value

A ggplot2 object

## Details

For retained: Light purple = "One Away" (passed one but not both), Dark
purple = "Retained" (passed both)
