# Calculate NA probabilities for resampling scenarios

Calculates the probability of successful mean calculation and the
probability of all values being NA for different counts of non-NA values
in a sample of size n. Useful for understanding the impact of missing
data on resampling.

## Usage

``` r
calculate_na_probabilities(n)
```

## Arguments

- n:

  (*integer*) The total sample size

## Value

A tibble with columns: non_na (count of non-NA values), na_count (count
of NA values), p_mean_success (probability of successfully calculating a
mean), and p_all_na (probability that all resampled values are NA)
