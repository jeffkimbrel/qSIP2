# Add isotopolog_label to source data

To convert from an isotope only dataframe to one with both isotope and
isotopolog_label to satisfy MISIP requirements

## Usage

``` r
add_isotopolog_label(data, isotope = "isotope")
```

## Arguments

- data:

  (*dataframe*) Sample metadata

- isotope:

  (*string, default: "isotope"*) Column name with isotope data

## Value

A dataframe with `isotopolog_label` column added and `isotope` column
modified

## See also

Other "MISIP":
[`remove_isotopolog_label()`](https://jeffkimbrel.github.io/qSIP2/reference/remove_isotopolog_label.md)
