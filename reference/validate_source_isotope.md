# Validate the given source mat ids have the expected labeled/unlabeled designation (internal)

Currently used by
[`run_feature_filter()`](https://jeffkimbrel.github.io/qSIP2/reference/run_feature_filter.md)
to make sure user given labeled or unlabeled source_mat_ids are not
incorrect with respective to their source_data.

## Usage

``` r
validate_source_isotope(qsip_data_object, source_mat_ids, isotope_list)
```

## Arguments

- qsip_data_object:

  A qsip_data object

- source_mat_ids:

  A character vector of source_mat_ids

- isotope_list:

  A character vector of isotopes to check against

## Value

TRUE (all match) or FALSE (some don't match)
