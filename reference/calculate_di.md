# Calculate death rate

Equation 6 from Koch, 2018

## Usage

``` r
calculate_di(
  N_light_it,
  N_total_i0,
  timepoint,
  timepoint1,
  growth_model = "exponential"
)
```

## Arguments

- N_light_it:

  The unlabeled copy number of feature i at timepoint t

- N_total_i0:

  The copy number of feature i at timepoint 0

- timepoint:

  The timepoint at which the copy number is being measured

- timepoint1:

  The timepoint that is being compared against

- growth_model:

  (*character, default: exponential*) The growth model to use. Must be
  either "exponential" or "linear"
