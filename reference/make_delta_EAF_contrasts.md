# Make an all-by-all contrasts table for delta EAF calculations

This is an internal function that runs if a user does not provide a
contrasts table to
[`run_delta_EAF_contrasts()`](https://jeffkimbrel.github.io/qSIP2/reference/run_delta_EAF_contrasts.md).
It will produce an all-by-all table and run through
[`validate_delta_EAF_contrasts()`](https://jeffkimbrel.github.io/qSIP2/reference/validate_delta_EAF_contrasts.md)
to modify the class.

## Usage

``` r
make_delta_EAF_contrasts(q)
```

## Arguments

- q:

  a qSIP2 list object

## Value

a dataframe
