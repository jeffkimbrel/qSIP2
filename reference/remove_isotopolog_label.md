# Remove isotopolog_label from "MISIPified" data

To convert from data with both isotope and isotopolog_label to one with
isotope only.

## Usage

``` r
remove_isotopolog_label(data)
```

## Arguments

- data:

  (*dataframe*) Sample metadata

## Value

A dataframe with `isotopolog_label` column removed and `isotope` column
modified

## See also

Other "MISIP":
[`add_isotopolog_label()`](https://jeffkimbrel.github.io/qSIP2/reference/add_isotopolog_label.md)
