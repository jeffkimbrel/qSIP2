# Get shared source_mat_ids and sample_ids in qSIP objects

This function finds the shared source_mat_ids between the `source_data`
and `sample_data`, and sample_ids between the `sample_data` and
`feature_data` objects. It also reports any ids that are unique to each
object.

If passing a `qsip_data` object as the first argument, no other
arguments are necessary. If a `qsip_source_data` is given, then the
`sample_data` and `feature_data` objects must be given as well.

Additionally, the results of `get_shared_ids()` might be obtained from
the `@shared` slot of a `qsip_data` object. This data can also be
"pretty printed" using the `get_unshared_ids(<qsip_data>)` function.

## Usage

``` r
get_shared_ids(source_data, sample_data = NULL, feature_data = NULL)
```

## Arguments

- source_data:

  (*qsip_source_data or qsip_data*) A qSIP object with source data
  object

- sample_data:

  (*qsip_sample_data*) A qSIP sample data object

- feature_data:

  (*qsip_feature_data*) A qSIP feature data object

## Value

A list with two lists, one for source_mat_ids and one for sample_ids.
