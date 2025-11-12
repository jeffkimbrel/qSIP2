# Get source_mat_ids meeting certain isotope conditions

Get source_mat_ids meeting certain isotope conditions

## Usage

``` r
get_all_by_isotope(
  qsip_data_object,
  isotopes,
  quiet = FALSE,
  silent = lifecycle::deprecated()
)
```

## Arguments

- qsip_data_object:

  (*qsip_data or qsip_source_data*) A qsip object with source data

- isotopes:

  (*string(s)*) Isotopes used to pull source_mat_ids. Can be a standard
  isotope name (e.g. `12C`) or special terms `labeled` or `unlabeled`

- quiet:

  (*boolean*) If `TRUE`, suppresses messages about missing isotope hits
  and doesn't fail

- silent:

  Deprecated, use `quiet` instead

## Value

A vector of source_mat_ids. It may also print some messages.
