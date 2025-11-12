# qSIP feature table class

The `qsip_feature_data` object holds validated feature metadata.

## Usage

``` r
qsip_feature_data(data, feature_id = "feature_id", type = "counts")
```

## Arguments

- data:

  (*dataframe*) ASV/OTU table or equivalent

- feature_id:

  (*string*) Column name with unique taxa IDs

- type:

  (*string, default: counts*) The type of numerical data, either
  *counts*, *coverage*, *normalized* or *relative*

## Value

A validated object of the `qsip_feature_data` type

## Details

`qsip_feature_data()` is not a typical function, but rather a class
constructor that instantiates a new `qsip_feature_data` object. The
constructor takes a `data.frame` as input and returns a validated
`qsip_feature_data` object.

The `qsip_feature_data` object is used to hold feature metadata, such as
amplicon sequence variants (ASVs), operational taxonomic units (OTUs),
metagenome-assembled genomes (MAGs), etc.

The `data` argument takes a `data.frame` that has the feature IDs as a
column designated with the `feature_id` argument. Each row corresponds
to a unique feature (amplicon, MAG, etc) and each subsequent row
corresponds to a unique sample.

The `type` argument is used to designate the type of data in the `data`
argument. It should most likely be *counts* for amplicon data, and
*coverage* for metagenome data (including normalizations like TPM). If
the data is relative abundances, the `type` argument should be set to
*relative*. Overall, the choice won't much affect the results from the
qSIP analysis, but choosing an accurate type will help with the
validation checks.

Internally, `qsip_feature_data` renames the metadata columns to be
standardized to MISIP terminology. A `data.frame` with the standardized
names can be extracted back out of the object using the
[`get_dataframe()`](https://jeffkimbrel.github.io/qSIP2/reference/get_dataframe.md)
method, and the optional `original_headers` argument can be set to
`TRUE` to return the original column names.

There are several validation checks run on the data on the `data.frame`:

- The `data` argument must contain a `data.frame`, including a tibble

- The `feature_id` argument must be a column name in the `data.frame`

- The `feature_id` column must contain unique values per row

- The `type` argument must be one of *counts*, *coverage* or *relative*

  - The `type` argument is *counts* by default, and in this case the
    values in the `data` argument must be integers

  - If `type` is set to *relative* the values in the `data` argument
    must be numeric and the values must sum to 1 for each row

  - If `type` is set to *coverage*, the values in the `data` argument
    must be numeric

  - If the `type` is set to *normalized* then the values are assumed to
    be pre-normalized and additional transformations will not be done.

- All values in the `data` argument must be non-negative

## See also

Other "qSIP Objects":
[`qsip_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_data.md),
[`qsip_sample_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_sample_data.md),
[`qsip_source_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_source_data.md)
