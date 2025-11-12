# Get counts of successful resampling

For each feature_id and label type, this function will return the counts
of successful resampling. This value will typically be the number of
resamples given to
[`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md),
but if
[`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md)
is called with `allow_failures = TRUE` then the number of successful
resamples might be less than the number of resamples given.

## Usage

``` r
get_resample_counts(qsip_data_object, as_percentage = FALSE)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object that has been resampled

- as_percentage:

  (*logical*) If TRUE, the counts will be returned as a percentage of
  the total number of resamples

## Value

A dataframe with columns feature_id, type, and n

## Details

If as_percentage is TRUE, the counts will be returned as a percentage of
the total number of resamples.
