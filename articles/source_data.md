# Source Data

``` r
library(dplyr)
library(qSIP2)
packageVersion("qSIP2")
#> [1] '0.21.1'
```

## Source Material and Metadata

In a SIP experiment, the “source material” are the subjects that you are
running experiments with (e.g. a culture tube or a plant root). For qSIP
the source material would then be each DNA extraction that is loaded
into its own column for isopycnic centrifugation. The “source data” is
the highest level of metadata with a row corresponding to each original
experimental or source material object. Because each source is
fractionated into samples, it will have a one-to-many relationship with
the “sample data” (see
[`vignette("sample_data")`](https://jeffkimbrel.github.io/qSIP2/articles/sample_data.md)).

There are a few required columns for valid source data including a
unique ID (the `source_mat_id`), some measure of quantitative abundance
for the source material (either total DNA or qPCR copies), and an
isotope and substrate designation (the `isotope` and `isotopolog`,
respectively). Ideally, the substrate should be a standardized compound
ID (e.g. PubChem [6137](https://pubchem.ncbi.nlm.nih.gov/compound/6137)
for L-methionine), but for `qSIP2` is can just be descriptive text like
“methionine”. In addition to the required columns, the source data can
contain as many other ancillary columns as necessary to describe the
experiment. These additional columns might contain important
experiment-specific metadata that you will use to group and subset your
source material in the qSIP workflow. But, they can also be further
details that you might not need for qSIP, but it may make sense to just
keep everything included if they’re already in your `.txt.` or excel
file.

An example source dataframe is included in the `qSIP2` package called
`example_source_df`. It includes a 13C glucose addition study with two
different moisture treatments (“normal” and “drought”) in quadruplicate,
with one only in triplicate. Each experiment contains both unlabeled 12C
and labeled 13C source material, but you may have an experiment where
different 13C treatments share the same 12C source material. For
example, a split experiment where you have one 12C data set that is
split into many experimental conditions each with a different
isotopolog.

| source | total_copies_per_g | total_dna | Isotope | Moisture | isotopolog |
|:-------|-------------------:|----------:|:--------|:---------|:-----------|
| S149   |           34838665 |  74.46539 | 12C     | Normal   | glucose    |
| S150   |           53528072 | 109.01522 | 12C     | Normal   | glucose    |
| S151   |           95774992 | 182.16852 | 12C     | Normal   | glucose    |
| S152   |            9126192 |  23.68963 | 12C     | Normal   | glucose    |
| S161   |           41744046 |  67.62552 | 12C     | Drought  | glucose    |
| S162   |           49402713 |  94.21217 | 12C     | Drought  | glucose    |

The first few rows of `example_source_df`

## qSIP2 Source Data Object

Once the dataframe is ready with at least the three required columns
(`source_mat_id`, `isotope` and `isotopolog`), the next step is to
convert it to a `qsip_source_data` object. This is one of the main
`qSIP2` objects to hold and validate the data.

``` r
source_object <- qsip_source_data(example_source_df,
  isotope = "Isotope",
  isotopolog = "isotopolog",
  source_mat_id = "source"
)

class(source_object)
#> [1] "qSIP2::qsip_source_data" "S7_object"
```

While these three columns are required for the EAF workflow, there are
additional columns required for the growth workflow (`timepoint`,
`total_abundance` and `volume`). These can remain empty/unassigned for
this vignette, and will be detailed in a forthcoming growth workflow
vignette.

Note, your column names in the dataframe don’t have to specifically be
the required column names, so no need to edit your original table
headers if they don’t match. For example, if you’re `isotopolog` column
is titled “substrate”, it isn’t necessary to rename your column. If your
column names are already standardized names, then there is no need to
assign while creating the object. For example, the `isotopolog` column
is already title “isotopolog”, so if it is omitted from the object
creation then the column will still be identified and used.

``` r
# this will still work even though the isotopolog parameter is not assigned
qsip_source_data(example_source_df,
  isotope = "Isotope",
  source_mat_id = "source"
)
```

### Structure of `qsip_source_data`

While this object is not meant to be inspected or worked with outside of
`qSIP2` functions, a quick
[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) can show
the structure of it.

``` r
glimpse(source_object)
#> <qSIP2::qsip_source_data>
#>  @ data           : tibble [15 × 6] (S3: tbl_df/tbl/data.frame)
#>  $ isotope           : chr [1:15] "12C" "12C" "12C" "12C" ...
#>  $ isotopolog        : chr [1:15] "glucose" "glucose" "glucose" "glucose" ...
#>  $ source_mat_id     : chr [1:15] "S149" "S150" "S151" "S152" ...
#>  $ total_copies_per_g: num [1:15] 34838665 53528072 95774992 9126192 41744046 ...
#>  $ total_dna         : num [1:15] 74.5 109 182.2 23.7 67.6 ...
#>  $ Moisture          : chr [1:15] "Normal" "Normal" "Normal" "Normal" ...
#>  @ isotope        : chr "Isotope"
#>  @ isotopolog     : chr "isotopolog"
#>  @ source_mat_id  : chr "source"
#>  @ timepoint      : chr "NULL"
#>  @ total_abundance: chr "NULL"
#>  @ volume         : chr "NULL"
```

The original dataframe is contained in the `@data` slot, however, some
column names have been modified to the standard names, while keeping a
record of the original names in the corresponding slots.

| Original Names | qSIP Names    | Original Name Slot |
|:---------------|:--------------|:-------------------|
| source         | source_mat_id | @source_mat_id     |
| Isotope        | isotope       | @isotope           |
| substrate      | isotopolog    | @isotopolog        |

Column name differences

To get the dataframe back out of the `qsip_source_data` object you can
use the
[`get_dataframe()`](https://jeffkimbrel.github.io/qSIP2/reference/get_dataframe.md)
method with `original_headers` set to `TRUE` or `FALSE`, depending on
your needs. But, note that the columns may be in a different order than
the dataframe you started with.

``` r
get_dataframe(source_object, original_headers = T)
```

| Isotope | isotopolog | source | total_copies_per_g | total_dna | Moisture |
|:--------|:-----------|:-------|-------------------:|----------:|:---------|
| 12C     | glucose    | S149   |           34838665 |  74.46539 | Normal   |
| 12C     | glucose    | S150   |           53528072 | 109.01522 | Normal   |
| 12C     | glucose    | S151   |           95774992 | 182.16852 | Normal   |
| 12C     | glucose    | S152   |            9126192 |  23.68963 | Normal   |
| 12C     | glucose    | S161   |           41744046 |  67.62552 | Drought  |
| 12C     | glucose    | S162   |           49402713 |  94.21217 | Drought  |
| 12C     | glucose    | S163   |           47777726 |  87.82524 | Drought  |
| 12C     | glucose    | S164   |           48734282 |  75.97274 | Drought  |
| 13C     | glucose    | S178   |           62964478 |  73.89526 | Normal   |
| 13C     | glucose    | S179   |           49475460 |  68.65182 | Normal   |
| 13C     | glucose    | S180   |           51720787 |  81.36874 | Normal   |
| 13C     | glucose    | S200   |           59426155 |  71.19377 | Drought  |
| 13C     | glucose    | S201   |           56379702 |  73.78225 | Drought  |
| 13C     | glucose    | S202   |           42562198 | 108.11436 | Drought  |
| 13C     | glucose    | S203   |           49914369 |  80.48608 | Drought  |

### Validation of `qsip_source_data`

While constructing a `qsip_source_data` object there are a few
validation checks that are performed. For now, the only checks are that
the `source_mat_id` is unique for each row, and that the `isotope` field
is an appropriate value. This doesn’t just mean it is a value that makes
sense, but also that it is one of the isotopes that `qSIP2` knows how to
calculate atom fraction values from. This is currently limited to
12C/13C, 14N/15N and 16O/18O. There are some “non-isotopic” names
allowed as well for source material that might be unfractionated. These
additional options are “bulk”, “unfractionated”, “T0”, “time0”, “Time0”,
and are added as exceptions in the
[`validate_isotopes()`](https://jeffkimbrel.github.io/qSIP2/reference/validate_isotopes.md)
helper function.

``` r

# artificially doubling the rows will give an error from duplicate source_mat_ids
example_source_df |>
  rbind(example_source_df) |>
  qsip_source_data(
    isotope = "Isotope",
    isotopolog = "isotopolog",
    source_mat_id = "source"
  )
#> Error: some source_mat_ids are duplicated
```

One benefit of the validation steps being embedded in the object itself
is that these validations are automatically run when the object is
modified. This makes it impossible to modify the data later to an
invalid object, e.g. changing an isotope to an invalid choice.

``` r
source_object@data$isotope <- "13G"
#> invalid isotope found: 13G
#> Error: Please fix the isotope names and try again
```

## MISIP

While qSIP standards are part of the MISIP[¹](#fn1) standards, the
`qSIP2` package is a little less stringent. This means your valid
`qSIP2` object might not be valid for a MISIP submission. At the source
data level this is primarily through the difference between how the
`isotope` data is coded, plus the addition of another `isotopolog_label`
column.

`qSIP2` has functions to convert between these two types.
`add_isotoplog_label()` makes a MISIP version of the source data, and
[`remove_isotopolog_label()`](https://jeffkimbrel.github.io/qSIP2/reference/remove_isotopolog_label.md)
converts it back to a `qSIP2` compatible version. Two things are changed
when running `add_isotoplog_label()` - 1) the `isotopolog_label` column
is added and is populated with either “isotopically labeled” or “natural
abundance” for heavy and light isotopes, respectively, and 2) the
`isotope` column gets modified to be only the heavy isotope (e.g. all
“12C” entries become “13C”).

Note, these functions are run on the source dataframe rather than on the
`qsip_source_data` object.

### Adding the `isotopolog_label` column

``` r
example_source_df |>
  add_isotopolog_label(isotope = "Isotope")
```

| source | total_copies_per_g | total_dna | isotope | isotopolog_label     | Moisture | isotopolog |
|:-------|-------------------:|----------:|:--------|:---------------------|:---------|:-----------|
| S151   |           95774992 | 182.16852 | 13C     | natural abundance    | Normal   | glucose    |
| S178   |           62964478 |  73.89526 | 13C     | isotopically labeled | Normal   | glucose    |
| S200   |           59426155 |  71.19377 | 13C     | isotopically labeled | Drought  | glucose    |
| S201   |           56379702 |  73.78225 | 13C     | isotopically labeled | Drought  | glucose    |
| S150   |           53528072 | 109.01522 | 13C     | natural abundance    | Normal   | glucose    |
| S180   |           51720787 |  81.36874 | 13C     | isotopically labeled | Normal   | glucose    |

A ’MISIPified version of `example_source_df`

Now, the `Isotope` column has been renamed to `isotope` to satisfy MISIP
standards, and all values have been replaced with the heavy isotope.

| isotope |   n |
|:--------|----:|
| 13C     |  15 |

Count of `isotope` types in `example_source_df_MISIP`

And the designation for whether the source material was the “light” or
“heavy” version of the isotope has now been transferred to the
`isotopolog_label` column.

| isotopolog_label     |   n |
|:---------------------|----:|
| isotopically labeled |   7 |
| natural abundance    |   8 |

Count of `isotopolog_label` types in `example_source_df_MISIP`

### Removing the `isotopolog_label` column

This change can be reverted with the
[`remove_isotopolog_label()`](https://jeffkimbrel.github.io/qSIP2/reference/remove_isotopolog_label.md)
function.

``` r
example_source_df |>
  add_isotopolog_label(isotope = "Isotope") |>
  remove_isotopolog_label()
```

| source | total_copies_per_g | total_dna | isotope | Moisture | isotopolog |
|:-------|-------------------:|----------:|:--------|:---------|:-----------|
| S149   |           34838665 |  74.46539 | 12C     | Normal   | glucose    |
| S150   |           53528072 | 109.01522 | 12C     | Normal   | glucose    |
| S151   |           95774992 | 182.16852 | 12C     | Normal   | glucose    |
| S152   |            9126192 |  23.68963 | 12C     | Normal   | glucose    |
| S161   |           41744046 |  67.62552 | 12C     | Drought  | glucose    |
| S162   |           49402713 |  94.21217 | 12C     | Drought  | glucose    |
| S163   |           47777726 |  87.82524 | 12C     | Drought  | glucose    |
| S164   |           48734282 |  75.97274 | 12C     | Drought  | glucose    |
| S178   |           62964478 |  73.89526 | 13C     | Normal   | glucose    |
| S179   |           49475460 |  68.65182 | 13C     | Normal   | glucose    |
| S180   |           51720787 |  81.36874 | 13C     | Normal   | glucose    |
| S200   |           59426155 |  71.19377 | 13C     | Drought  | glucose    |
| S201   |           56379702 |  73.78225 | 13C     | Drought  | glucose    |
| S202   |           42562198 | 108.11436 | 13C     | Drought  | glucose    |
| S203   |           49914369 |  80.48608 | 13C     | Drought  | glucose    |

`example_source_df_MISIP` converted back

Note, the original is not *exactly* preserved as the original `Isotope`
column has the MISIP standard `isotope` name retained.

------------------------------------------------------------------------

1.  <https://www.biorxiv.org/content/10.1101/2023.07.13.548835v1>
