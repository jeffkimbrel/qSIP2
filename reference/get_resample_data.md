# Get dataframe of resampled data

This function will return a dataframe of the resampled data embedded in
a `qsip_data` object.

## Usage

``` r
get_resample_data(qsip_data_object, type = "all", pivot = FALSE)
```

## Arguments

- qsip_data_object:

  A `qsip_data` object that has been run through
  [`run_resampling()`](https://jeffkimbrel.github.io/qSIP2/reference/run_resampling.md)

- type:

  (*string*) The type of data to return: "all", "unlabeled", or
  "labeled"

- pivot:

  (*boolean*) Whether to pivot the data into a long format or keep as
  wide

## Value

A dataframe of the resampled data
