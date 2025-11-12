# qSIP sample data class

The `qsip_sample_data` object holds validated sample metadata.

## Usage

``` r
qsip_sample_data(
  data,
  sample_id = "sample_id",
  source_mat_id = "source_mat_id",
  gradient_position = "gradient_position",
  gradient_pos_density = "gradient_pos_density",
  gradient_pos_amt = "gradient_pos_amt",
  gradient_pos_rel_amt = "",
  convert_negatives = FALSE,
  overwrite = FALSE
)
```

## Arguments

- data:

  (*dataframe*) Metadata for samples/fractions

- sample_id:

  (*string*) The unique sample ID

- source_mat_id:

  (*string*) The unique ID for the biological subject or replicate

- gradient_position:

  (*string*) Column name with the fraction position

- gradient_pos_density:

  (*string*) Column name with the gradient density

- gradient_pos_amt:

  (*string*) Column name with a total amount per fraction, either qPCR
  copies or DNA

- gradient_pos_rel_amt:

  (*string*) Column name with the relative fraction abundance compared
  to the total

- convert_negatives:

  (*boolean, default: FALSE*) Whether or not to convert negative amounts
  to zero

- overwrite:

  (*boolean, default: FALSE*) Whether or not to overwrite the
  gradient_pos_rel_amt values

## Value

A validated object of the `qsip_sample_data` type

## Details

`qsip_sample_data()` is not a typical function, but rather a class
constructor that instantiates a new `qsip_sample_data` object. The
constructor takes a `data.frame` as input and returns a validated
`qsip_sample_data` object.

In qSIP and MISIP, a "sample" is the post-fractionated material with
metadata pertaining to the fractionation process. Sample metadata
contains information about the sample and fractionation, such as the
sample ID, the source material ID, the gradient position, the density,
the amount recovered (e.g. DNA concentration or 16S copies), and the
relative abundance of the fraction compared to the total.

Ideally, `gradient_pos_amt` should be reported as a mass value of DNA
rather than a concentration. However, if the concentration is reported,
the `fraction_volume` argument can be used to convert the
`gradient_pos_amt` concentration to a mass value. For example, if the
`gradient_pos_amt` is reported as ng/ul, and the `fraction_volume` is
reported as 100 ul, then the `gradient_pos_amt` will be converted to ng.

Internally, `qsip_sample_data` renames the metadata columns to be
standardized to MISIP terminology. A `data.frame` with the standardized
names can be extracted back out of the object using the
[`get_dataframe()`](https://jeffkimbrel.github.io/qSIP2/reference/get_dataframe.md)
method, and the optional `original_headers` argument can be set to
`TRUE` to return the original column names.

There are several validation checks done on the `data.frame`:

- The `data` argument must contain a `data.frame`, including a tibble

- The `sample_id` column must contain unique values per row

- The `gradient_position` must container positive integers, or `-1` is
  allowed to designate the sample as "bulk" or unfractionated

## See also

Other "qSIP Objects":
[`qsip_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_data.md),
[`qsip_feature_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_feature_data.md),
[`qsip_source_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_source_data.md)
