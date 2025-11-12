# Calculate birth rate

Equation 7 from Koch, 2018

## Usage

``` r
calculate_bi(
  N_total_it,
  N_light_it,
  timepoint,
  timepoint1,
  growth_model = "exponential"
)
```

## Arguments

- N_total_it:

  The total copy number of feature i at timepoint t

- N_light_it:

  The unlabeled copy number of feature i at timepoint t

- timepoint:

  The timepoint at which the copy number is being measured

- timepoint1:

  The timepoint that is being compared against

- growth_model:

  (*character, default: exponential*) The growth model to use. Must be
  either "exponential" or "linear"
