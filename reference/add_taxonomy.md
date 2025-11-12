# Add a taxonomy table to qSIP abundance data

This function is designed to add taxonomy data to a feature table, but
in reality it can hold any metadata that you want to associate with your
features. The only requirement is that the metadata table must have a
feature id column with values that match the feature ids in the
`qsip_feature_data` object, and these ids must not be duplicated.

## Usage

``` r
add_taxonomy(feature_object, taxa, feature_id)
```

## Arguments

- feature_object:

  (*qsip_feature_data*) An object of `qsip_feature_data` class

- taxa:

  (*dataframe*) A taxa table

- feature_id:

  (*string*) The column name for the feature ids that match the ids in
  the abundance table

## Value

An updated `qsip_feature_data` with the taxonomy slot populated with a
taxonomy dataframe.
