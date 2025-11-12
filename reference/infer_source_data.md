# Generate a source data frame from a sample data frame

There may be situations where you have a verbose sample data frame that
has all of the data for both the samples and the source materials. This
function attempts to infer a source data frame from the sample data
frame. It does this by grouping by the source matrix ID and then
checking to see if all of the values in each column are the same.If they
are, then that column is kept in the source data frame. If not, then
that column is dropped.

## Usage

``` r
infer_source_data(sample_data, source_mat_id)
```

## Arguments

- sample_data:

  (*dataframe*) A data frame with combined sample and source data

- source_mat_id:

  (*string*) The column with source_mat_id

## Value

A data frame with the inferred source data

## See also

Other "Sample Data":
[`add_gradient_pos_rel_amt()`](https://jeffkimbrel.github.io/qSIP2/reference/add_gradient_pos_rel_amt.md)
