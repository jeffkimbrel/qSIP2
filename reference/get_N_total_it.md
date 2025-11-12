# Calculate total abundances at timepoint t

This function takes a `qsip_data` object and calculates the total
abundance of each feature at time zero. This should be done on an early
`qsip_data` object that still has time zero data.

## Usage

``` r
get_N_total_it(qsip_data_object, timepoint = "timepoint", t = 0, group = NULL)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) An object of `qsip_data` class

- timepoint:

  (*character*) The name of the timepoint column in the source data

- t:

  (*numeric*) The value of the timepoint column to filter on

- group:

  (*character*) The name of the grouping variable(s) to summarize the
  counts

## Value

(*data.frame*) A data frame with feature_id and total abundance at time
zero

## Details

Sometimes, a feature will have abundance at a later time point, but no
values for time zero. If a feature has zero abundance at time zero, a
warning will be issued, but the feature will still be included in the
output with a starting abundance of zero.
