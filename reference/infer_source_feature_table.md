# Infer source feature table from sample counts

This function will convert the tube_relative_abundance data for each
feature into a copy number that was measured for the source material.
The resulting table will have values in the original units of the
`copy_number_col` parameter. In other words, if the `copy_number_col` is
"16S copies per g/soil", then the output will be in copies of each
feature per g/soil.

## Usage

``` r
infer_source_feature_table(
  qsip_data_object,
  copy_number_col = NULL,
  total_copies = NULL
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) An object of `qsip_data` class

- copy_number_col:

  (*string*) column in the original source metadata with feature copy
  number

- total_copies:

  (*integer*) Scale the copy numbers to this value (overwrites
  `copy_number_col`)

## Value

a feature tibble
