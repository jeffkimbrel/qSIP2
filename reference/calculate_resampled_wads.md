# Internal function for resampling WAD values (internal)

Designed as a purrr::map() function called by run_resampling.R

## Usage

``` r
calculate_resampled_wads(i, wad_data, type, allow_failures = FALSE)
```

## Arguments

- i:

  (*integer*) The specific iteration of the resampling

- wad_data:

  (*dataframe*) A WAD dataframe to resample columns from

- type:

  (*string*) Text for whether the wad data is from labeled or unlabeled
  data

- allow_failures:

  (*logical*) Whether to allow failures in the resampling

## Value

The resampling data that will be boot in `@resamples`
