# Calculate weighted average density (WAD) values (internal)

Calculate weighted average density (WAD) values (internal)

## Usage

``` r
calculate_wads(tube_rel_abundance)
```

## Arguments

- tube_rel_abundance:

  (*dataframe*) Output from
  [`calculate_tube_rel_abundance()`](https://jeffkimbrel.github.io/qSIP2/reference/calculate_tube_rel_abundance.md)

## Value

A list with two objects, 1) a dataframe with WAD info for the
feature_ids found in at least one sample, and 2) the fraction counts for
all feature_ids, including those not found at all in some samples. It
also prints a message to the screen with a count of feature_ids that are
entirely missing from at least one sample.
