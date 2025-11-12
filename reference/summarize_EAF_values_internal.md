# Internal function to summarize EAF values

Internal function to summarize EAF values

## Usage

``` r
summarize_EAF_values_internal(
  qsip_data_object,
  taxonomy = FALSE,
  confidence = 0.9
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip_data object

- taxonomy:

  (*logical, default: FALSE*) If TRUE, add the taxonomy of the
  feature_id

- confidence:

  (*numeric, default: 0.9*) The confidence level for the confidence
  interval

  Called by `summarize_EAF_values` to calculate the resampled EAF
  values.
