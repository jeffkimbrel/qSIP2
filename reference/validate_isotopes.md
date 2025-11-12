# Check the validity of an isotope string (internal)

Often the "bulk" designation is found in this column, so those can
optionally be removed from validation checks

## Usage

``` r
validate_isotopes(
  isotope,
  isotope_list = c(valid_isotope_names$unlabeled, valid_isotope_names$labeled),
  unfractionated_terms = valid_isotope_names$unfractionated
)
```

## Arguments

- isotope:

  (*string(s)*) Isotope value or values

- isotope_list:

  (*strings, default: c("12C", "13C", "14N", "15N", "16O", "18O")*)
  Isotopes to check against

- unfractionated_terms:

  (*strings*) Terms to ignore when checking isotope values

## Value

Returns `NULL` if the isotope strings are valid, or a printed error

## Note

The isotope_list may change if isotopolog_label stays a thing. Only the
"labeled" isotopes will be allowed.
