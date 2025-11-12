# Calculate EAF values

This is the last main workhorse function in qSIP analysis and generates
the important values (Z, G, M, M_labeledmax, M_labeled and EAF) for the
observed data as well as for all of the resampled data.

## Usage

``` r
run_EAF_calculations(
  qsip_data_object,
  gc_method = "MM",
  propO = lifecycle::deprecated()
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip_data_object with resample information

- gc_method:

  (*string*) The method to use for calculating the GC content from WAD

- propO:

  deprecated

## Value

Returns an updated `qsip_data_object` with final EAF and other values in
the `@EAF` slot.
