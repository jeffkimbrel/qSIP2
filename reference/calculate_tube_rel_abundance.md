# Calculate tube relative abundance (internal)

The "tube level relative abundance" has two layers of relative
abundance. It first takes the `raw_abundance` counts per sample, and
divides by the total to get the `rel_abundance` values. Next, it takes
these values and divides them by the `gradient_pos_rel_amt` values
stored at the source level which normalizes the `rel_abundance` values
by the amount of sample per fraction. This is the final value stored in
the `tube_rel_abundance` column.

## Usage

``` r
calculate_tube_rel_abundance(source_data, sample_data, feature_data)
```

## Arguments

- source_data:

  (*qsip_source_data*) A qSIP source data object

- sample_data:

  (*qsip_sample_data*) A qSIP sample data object

- feature_data:

  (*qsip_feature_data*) A qSIP feature data object

## Value

A long format dataframe with one row per `feature_id` per `sample_id`

## Details

To speed up the calculations, this function removes `feature_ids` in a
sample if the abundance is zero.

v0.10.3 updated this function to have different behavior for feature
type 'normalized'. This type should use the raw data in the feature
table, putting those values into the `tube_rel_abundance` column.
