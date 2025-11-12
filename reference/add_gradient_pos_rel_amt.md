# Add gradient_pos_rel_amt to data

This function will calculate the relative amount of a fraction compared
to the whole source using either qPCR copies or DNA concentrations. This
works by summing all of the within a source, and then dividing each
fraction by the total. The total percent will always equal 1 (or 100%)
when running this function.

## Usage

``` r
add_gradient_pos_rel_amt(
  data,
  amt,
  source_mat_id = "source_mat_id",
  overwrite = FALSE
)
```

## Arguments

- data:

  (*dataframe*) Sample metadata

- amt:

  (*string*) Column name that has the qPCR or DNA amounts per fraction

- source_mat_id:

  (*string, default: "source_mat_id"*) Column name with the
  source_mat_id

- overwrite:

  (*bool, default: FALSE*) Determines whether or not to overwrite an
  existing gradient_pos_rel_amt column

## Value

A dataframe with a `gradient_pos_rel_amt` column

## Details

There may be times that you would want to provide values that do not sum
to 100%. For example, if you throw out a certain fraction because it
didn't sequence well, then your total could be less than 100%.

## See also

Other "Sample Data":
[`infer_source_data()`](https://jeffkimbrel.github.io/qSIP2/reference/infer_source_data.md)
