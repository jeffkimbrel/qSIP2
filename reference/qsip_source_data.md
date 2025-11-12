# qSIP source data class

The `qsip_source_data` object holds validated source material metadata.

## Usage

``` r
qsip_source_data(
  data,
  isotope = "isotope",
  isotopolog = "isotopolog",
  source_mat_id = "source_mat_id",
  timepoint = "NULL",
  total_abundance = "NULL",
  volume = "NULL"
)
```

## Arguments

- data:

  (*dataframe*) Source metadata

- isotope:

  (*string*) Isotope name

- isotopolog:

  (*string*) Isotopolog data

- source_mat_id:

  (*string*) The unique ID for the biological subject or source

- timepoint:

  (*string*) Timepoint data

- total_abundance:

  (*string*) Total abundance data

- volume:

  (*string*) Volume of the abundance data. Defaults to 1, but can be a
  ul volume if abundance data is given as a concentration

## Value

A validated `qsip_source_data` object

## Details

`qsip_source_data()` is not a typical function, but rather a class
constructor that instantiates a new `qsip_source_data` object. The
constructor takes a `data.frame` as input and returns a validated
`qsip_source_data` object.

In qSIP and MISIP, "source material" is your original biological
specimen that DNA was extracted from. This could be a soil sample, a
plant, a mouse, etc. This is the pre-fractionated metadata, and
post-fractionation metadata goes in the `qsip_sample_data` object.

Several validation checks are run on the input data:

- The `data` argument must be a `data.frame`, including a tibble

- The `isotope`, `isotopolog`, and `source_mat_id` arguments must be
  column names in the `data.frame`

- The `source_mat_id` column must be unique

- The `isotope` column must contain valid isotope names. "Valid" means
  they must be one of the types that the `qSIP2` package has equations
  for, namely 12C/13C, 14N/15N and 16O/18O. Some non-isotope names are
  also valid, including "bulk", "unfractionated" and "T0".

Internally, `qsip_source_data` renames the metadata columns to be
standardized to MISIP terminology. A `data.frame` with the standardized
names can be extracted back out of the object using the
[`get_dataframe()`](https://jeffkimbrel.github.io/qSIP2/reference/get_dataframe.md)
method, and the optional `original_headers` argument can be set to
`TRUE` to return the original column names.

One column of metadata that is required although not used by `qSIP2` is
the `isotopolog` column. This column is required to capture complete
metadata that is compliant with the MISIP standards. However, when
running experiments with multiple isotopologs this column can be used to
generate correct comparison groups using the
[`get_comparison_groups()`](https://jeffkimbrel.github.io/qSIP2/reference/get_comparison_groups.md)
function.

## See also

Other "qSIP Objects":
[`qsip_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_data.md),
[`qsip_feature_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_feature_data.md),
[`qsip_sample_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_sample_data.md)
