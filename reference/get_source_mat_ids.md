# Return the source_mat_ids in a qsip object

Return the source_mat_ids in a qsip object

## Usage

``` r
get_source_mat_ids(qsip_data_object, filtered = FALSE)
```

## Arguments

- qsip_data_object:

  A `qSIP_data` object that has been run through
  [`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md)

- filtered:

  (*Boolean*) If TRUE, return the feature_ids from the filtered data
