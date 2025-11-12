# Remove isotopolog label if needed

The qSIP2 functions expect non-MISIP data. If running qSIP2 within
KBase, then this function will verify/update the data to be
non-MISIPified.

## Usage

``` r
remove_isotopolog_label_check(df, isotope = "isotope")
```

## Arguments

- df:

  Sample data

- isotope:

  Column name with isotope data
