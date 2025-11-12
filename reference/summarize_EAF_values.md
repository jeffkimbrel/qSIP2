# Summarize the observed and resampled EAF values

Reports observed EAF value (`observed_EAF`) as well as the mean of the
resampled values (`mean_resampled_EAF`) and the `lower` and `upper`
confidence interval with a given `confidence` limit.

## Usage

``` r
summarize_EAF_values(
  qsip_data_object,
  confidence = 0.9,
  taxonomy = FALSE,
  quiet = FALSE
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip_data object or list of qsip_data objects

- confidence:

  (*numeric, default: 0.9*) The confidence level for the confidence
  interval

- taxonomy:

  (*logical, default: FALSE*) If TRUE, add the taxonomy of the
  feature_id

- quiet:

  (*logical, default: FALSE*) Suppress messages

## Value

A `dataframe` with summarized observed and resampled EAF values

## Details

The confidence interval uses the resampling method where it returns the
quantile values from the resampled data. If `confidence = 0.9` (the
default) then this function returns the 5% and 95% quantiles
(representing 90% of the resampling) as the `lower` and `upper` results.
