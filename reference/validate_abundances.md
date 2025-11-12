# Check the validity of a feature abundance table (internal)

This validation function is an internal function will check the validity
of a feature abundance table.

## Usage

``` r
validate_abundances(data, feature_id, type)
```

## Arguments

- data:

  (*dataframe*) ASV/OTU table

- feature_id:

  (*string*) Column name with feature IDs

- type:

  (*string*) *counts* requires integers, *coverage* and *relative* can
  take any positive numeric

## Value

Returns `NULL` if the values are valid, or a printed error

## Details

Rows should contain the unique taxa ids with a column designated with
the id argument. Each other column name should be a unique sample name.

Validity checking includes making sure all data is numeric (except for
the feature IDs), all numbers are integers (if `type = "counts"`), and
no numbers are negative.
