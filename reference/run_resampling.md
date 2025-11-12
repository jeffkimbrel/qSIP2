# Resample WAD values

Takes a filtered WAD dataframe and resamples x times

## Usage

``` r
run_resampling(
  qsip_data_object,
  resamples = 1000,
  with_seed = NULL,
  allow_failures = FALSE,
  progress = TRUE,
  quiet = FALSE
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object that has been filtered

- resamples:

  (*integer*) The number of resamples/bootstraps to run

- with_seed:

  (*integer*) An optional seed for reproducibility

- allow_failures:

  (*boolean*) Option to allow resampling failures. If TRUE, the function
  will continue to resample even if some features fail. If FALSE, the
  function will stop if any features fail.

- progress:

  (*boolean*) Option to show a progress bar for the resampling step

- quiet:

  (*boolean*) Option to suppress messages

## Value

A new `qsip_data` object with the `@resamples` slot populated with
resamples wad values

## Details

This function returns a list of resampled dataframes of x length for
both the labeled and unlabeled sources.
