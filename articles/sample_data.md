# Sample Data

``` r
library(dplyr)
library(qSIP2)
packageVersion("qSIP2")
#> [1] '0.21.2'
```

## Samples and Metadata

In `qSIP2`, “sample data” refers to any metadata associated with the
individual fractions or their sequencing data. This is the second level
of metadata - more granular than “source data”, but at a higher-level
than the “feature data”.

The word **sample** typically refers to the biological or environmental
entity the DNA was isolated (aka the **source material**). In qSIP,
however, because there are multiple sequencing runs per **source**, the
term **sample** has historically been reserved for sequencing of each
fraction. In practice, this means you will have many **samples** for
each single **source**. To keep with the proposed MISIP[¹](#fn1)
standards, the source material is coded with the `source_mat_id`, and
each sequencing run/fraction is coded with the `sample_id`.

The sample data has the most requirements to pass validation of the
three `qSIP2` input types. To standardize these requirements the column
names should adhere as closely to MISIP standards where possible.
Absolute requirements are slightly different than the official MISIP
standards, and for the `qSIP2` package would be the `source_mat_id`,
`sample_id`, `gradient_position`, `gradient_pos_density` and
`gradient_pos_rel_amt` fields.

### `source_mat_id` and `sample_id` columns

One column of the metadata should contain a unique `sample_id` that is
associated with a `source_mat_id` that matches one row on the
`qsip_source_data` dataframe. Although the `sample_id` should be unique
for each row, it is expected that the `source_mat_id` will be duplicated
creating a **many-to-one** relationship. The format of the `sample_id`
doesn’t matter, as long as it is unique. A popular choice might be the
`source_mat_id` appended with the gradient position.

### `gradient_position` and `gradient_pos_density` columns

These are the required columns to describe the position and density
values for each `sample_id`. `gradient_position` is typically numbered
by decreasing density, so the heaviest will be `1`, second heaviest will
be `2`, etc. To pass `qSIP2` validation the `gradient_position` must be
a positive integer, but `-1` will also be accepted to designate a “bulk”
or other “unfractionated” sample (e.g. when doing growth analysis).
`gradient_position` is not used directly in `qSIP2` calculations but can
be useful for quality control. `gradient_pos_density` is reported as
g/ml with as much decimal place precision as your method allows.

### `gradient_pos_amt` and `gradient_pos_rel_amt` columns

A requirement for qSIP is the `gradient_pos_rel_amt` field, which gives
the percent amount that a fraction has of the whole. The preferred
method is given in qPCR copy numbers per volume of source material, but
DNA totals/concentrations can be used as well.

The `gradient_pos_rel_amt` column is required for MISIP standards, but
for `qSIP2` we also require an additional `gradient_pos_amt` column with
the “raw” abundance values for each sample. This requirement is because
typically a qSIP workflow will produce the `gradient_pos_amt` values,
and the `gradient_pos_rel_amt` can be derived from it using the
[`add_gradient_pos_rel_amt()`](https://jeffkimbrel.github.io/qSIP2/reference/add_gradient_pos_rel_amt.md)
function. This function simply totals up the `gradient_pos_amt` for each
sample in a source, and then divides by the total. This math is the same
whether using qPCR amplicon copies or total DNA isolated.

For example, if there are 100,000 total 16S copies in a source as
determined by qPCR, and 15,000 copies in fraction 7, then the
`gradient_pos_rel_amt` value for fraction 7 would be 0.15 (15,000 /
100,000). Similarly, if you had 25ng total DNA used for density
separation, and fraction 7 had 3.75 ng DNA recovered, then
`gradient_pos_rel_amt` would also be 0.15 (3.75 / 25).

See below for cases where it might make sense for the totals to not add
up to 100%, and how to work with those cases in `qSIP2`.

An example sample dataframe is included in the `qSIP2` package to
demonstrate the possible columns.

``` r
example_sample_df
```

| sample | source | Fraction | density_g_ml |  dna_conc | avg_16S_g_soil |
|:-------|:-------|---------:|-------------:|----------:|---------------:|
| 149_F1 | S149   |        1 |     1.778855 | 0.0000000 |      4473.7081 |
| 149_F2 | S149   |        2 |     1.773391 | 0.0000000 |       986.6581 |
| 149_F3 | S149   |        3 |     1.765742 | 0.0000000 |      4002.7026 |
| 149_F4 | S149   |        4 |     1.759185 | 0.0000000 |      3959.7283 |
| 149_F5 | S149   |        5 |     1.752629 | 0.0012413 |      5725.7319 |
| 149_F6 | S149   |        6 |     1.746072 | 0.0128156 |      7566.2722 |

First few rows of formatted sample data

## qSIP2 Sample Data Object

The
[`qsip_sample_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_sample_data.md)
constructor builds a sample data object from the dataframe. Like the
source data object, the sample data object is built by providing column
names to the appropriate parameters.

``` r
sample_object <- qsip_sample_data(example_sample_df,
  sample_id = "sample",
  source_mat_id = "source",
  gradient_position = "Fraction",
  gradient_pos_density = "density_g_ml",
  gradient_pos_amt = "avg_16S_g_soil"
)
#> <gradient_pos_rel_amt> not specified. Calculating using avg_16S_g_soil column
```

Notice our dataframe did not contain the required `gradient_pos_rel_amt`
column, but the function was able to calculate it using the data in the
`gradient_pos_amt` column. See below for reasons why you might want to
pre-calculate this value rather than having the function calculate it
for you.

### Structure of `qsip_sample_data`

Like other `qSIP2` objects, the `qsip_sample_data` object is a list with
a few key components, but not meant to be inspected directly.

``` r
glimpse(sample_object)
#> <qSIP2::qsip_sample_data>
#>  @ data                : tibble [284 × 7] (S3: tbl_df/tbl/data.frame)
#>  $ sample_id           : chr [1:284] "149_F1" "149_F2" "149_F3" "149_F4" ...
#>  $ source_mat_id       : chr [1:284] "S149" "S149" "S149" "S149" ...
#>  $ gradient_position   : int [1:284] 1 2 3 4 5 6 7 8 9 10 ...
#>  $ gradient_pos_density: num [1:284] 1.78 1.77 1.77 1.76 1.75 ...
#>  $ gradient_pos_amt    : num [1:284] 4474 987 4003 3960 5726 ...
#>  $ gradient_pos_rel_amt: num [1:284] 1.28e-04 2.83e-05 1.15e-04 1.14e-04 1.64e-04 ...
#>  $ dna_conc            : num [1:284] 0 0 0 0 0.00124 ...
#>  @ sample_id           : chr "sample"
#>  @ source_mat_id       : chr "source"
#>  @ gradient_position   : chr "Fraction"
#>  @ gradient_pos_density: chr "density_g_ml"
#>  @ gradient_pos_amt    : chr "avg_16S_g_soil"
#>  @ gradient_pos_rel_amt: chr "gradient_pos_rel_amt"
```

The dataframe can be returned from the object using the
[`get_dataframe()`](https://jeffkimbrel.github.io/qSIP2/reference/get_dataframe.md)
function.

``` r
get_dataframe(sample_object)
```

| sample_id | source_mat_id | gradient_position | gradient_pos_density | gradient_pos_amt | gradient_pos_rel_amt |  dna_conc |
|:----------|:--------------|------------------:|---------------------:|-----------------:|---------------------:|----------:|
| 149_F1    | S149          |                 1 |             1.778855 |        4473.7081 |            0.0001284 | 0.0000000 |
| 149_F2    | S149          |                 2 |             1.773391 |         986.6581 |            0.0000283 | 0.0000000 |
| 149_F3    | S149          |                 3 |             1.765742 |        4002.7026 |            0.0001149 | 0.0000000 |
| 149_F4    | S149          |                 4 |             1.759185 |        3959.7283 |            0.0001137 | 0.0000000 |
| 149_F5    | S149          |                 5 |             1.752629 |        5725.7319 |            0.0001643 | 0.0012413 |
| 149_F6    | S149          |                 6 |             1.746072 |        7566.2722 |            0.0002172 | 0.0128156 |

First few rows of formatted sample data

### Validation of `qsip_sample_data`

A valid `qsip_sample_data` object will have the required columns,
`sample_id` will contain unique values, and `gradient_position` will be
positive integers or `-1`.

Additionally, the `density_g_ml` values should be in a reasonable range
(between 1.55 and 1.8)

``` r
# modifying density_g_ml to be too low will give an error
example_sample_df |>
  mutate(density_g_ml = density_g_ml / 2) |>
  qsip_sample_data(
    sample_id = "sample",
    source_mat_id = "source",
    gradient_position = "Fraction",
    gradient_pos_density = "density_g_ml",
    gradient_pos_amt = "avg_16S_g_soil"
  )
#> <gradient_pos_rel_amt> not specified. Calculating using avg_16S_g_soil column
#> Error in validate_gradient_pos_density(dplyr::select(self@data, gradient_position, : some gradient_pos_density values are lower than 1.55
```

## When to calculate the `gradient_pos_rel_amt` values?

Typically all of the `gradient_pos_rel_amt` for a given `source_mat_id`
should add up to 1 (i.e. 100%), but there are situations where it might
be less than 1 (although never greater than). For example, if you
removed some fractions because they didn’t sequence well or there was
some other reason to remove a fraction. If those removed fractions were
5% of the total data in that `source_mat_id`, then you would expect the
total of all fractions to be 0.95 rather than 1. Another situation would
be that although you added 25ng to a centrifugation, you only recovered
20ng and the rest was lost[²](#fn2).

This matters because if you run the
[`add_gradient_pos_rel_amt()`](https://jeffkimbrel.github.io/qSIP2/reference/add_gradient_pos_rel_amt.md)
function on your dataframe then the totals will add up to 1. You can
then remove the rows from the dataframe that you don’t want to include
in the analysis, and sum of the `gradient_pos_rel_amt` values will equal
the appropriately adjusted amount. But, if you remove the bad rows from
the dataframe first, and then let the
[`qsip_sample_data()`](https://jeffkimbrel.github.io/qSIP2/reference/qsip_sample_data.md)
function create the `gradient_pos_rel_amt` values, then they will be
artificially higher than they should be.

You can use a total abundance (qPCR or DNA concentrations) to calculate
the `gradient_pos_rel_amt` column.

``` r
add_gradient_pos_rel_amt(example_sample_df,
  source_mat_id = "source",
  amt = "avg_16S_g_soil"
)
```

| sample | source | Fraction | density_g_ml |  dna_conc | avg_16S_g_soil | gradient_pos_rel_amt |
|:-------|:-------|---------:|-------------:|----------:|---------------:|---------------------:|
| 149_F1 | S149   |        1 |     1.778855 | 0.0000000 |      4473.7081 |            0.0001284 |
| 149_F2 | S149   |        2 |     1.773391 | 0.0000000 |       986.6581 |            0.0000283 |
| 149_F3 | S149   |        3 |     1.765742 | 0.0000000 |      4002.7026 |            0.0001149 |
| 149_F4 | S149   |        4 |     1.759185 | 0.0000000 |      3959.7283 |            0.0001137 |
| 149_F5 | S149   |        5 |     1.752629 | 0.0012413 |      5725.7319 |            0.0001643 |
| 149_F6 | S149   |        6 |     1.746072 | 0.0128156 |      7566.2722 |            0.0002172 |

sample data with `gradient_pos_rel_amt` added

Trying to run this function on a dataframe with an existing
`gradient_pos_rel_amt` will give an error, but it can be overridden with
the `overwrite = T` flag.

``` r
add_gradient_pos_rel_amt(example_sample_df,
  source_mat_id = "source",
  amt = "avg_16S_g_soil"
) |>
  add_gradient_pos_rel_amt(
    source_mat_id = "source",
    amt = "avg_16S_g_soil"
  )
#> Error: gradient_pos_rel_amt already exists! Set overwrite = TRUE if you want to overwrite
```

``` r
# set overwrite = TRUE to override the error, although this is a silly example here
add_gradient_pos_rel_amt(example_sample_df,
  source_mat_id = "source",
  amt = "avg_16S_g_soil"
) |>
  add_gradient_pos_rel_amt(
    source_mat_id = "source",
    amt = "avg_16S_g_soil",
    overwrite = TRUE
  )
```

------------------------------------------------------------------------

1.  <https://www.biorxiv.org/content/10.1101/2023.07.13.548835v1>

2.  [The angel’s share and devil’s
    cut](https://www.visitlex.com/guides/post/common-geeky-bourbon-terms-explained/#:~:text=Angel's%20Share)
