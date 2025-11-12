# qSIP master data class

The `qsip_data` object holds validated qSIP source, sample and feature
metadata, and has slots to store all of the subsequence `qSIP2`
analysis.

## Usage

``` r
qsip_data(source_data, sample_data, feature_data)
```

## Arguments

- source_data:

  (*qsip_source_data*) A qSIP source data object

- sample_data:

  (*qsip_sample_data*) A qSIP sample data object

- feature_data:

  (*qsip_feature_data*) A qSIP feature data object

## Value

A validated `qsip_data` object

## Details

`qsip_data()` is not a typical function, but rather a class constructor
that instantiates a new `qsip_data` object. The constructor takes a
`qsip_source_data`, `qsip_sample_data` and `qsip_feature_data` as input
and returns a validated `qsip_data` object.

This `qsip_data` object holds the source, sample and feature data. It
also creates empty slots to hold the filtering results, the resampling
and the EAF values from their associated functions. For this reason, the
`qsip_data` object is intended to be progressively overwritten with new
analysis results, but new objects can be created at any point in the
analysis, if necessary. For example, a study with multiple comparison
groups might be combined into one large `qSIP_data` object, and then
split into separate objects at the `run_feature_filtering()` step.

Internally, creating the original qSIP objects renames the metadata
columns to be standardized to MISIP terminology. A `data.frame` with the
standardized names can be extracted back out of the `qSIP_data` using
the
[`get_dataframe()`](https://jeffkimbrel.github.io/qSIP2/reference/get_dataframe.md)
method and a required `type` argument of "source", "sample" or
"feature". The optional `original_headers` argument can be set to `TRUE`
to return the original column names.

## See also

Other "qSIP Objects":
[`qsip_feature_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_feature_data.md),
[`qsip_sample_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_sample_data.md),
[`qsip_source_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_source_data.md)
