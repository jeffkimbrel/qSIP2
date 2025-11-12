# Check the validity of gradient position values (internal)

Valid gradient positions are integers. The value can be `-1` to
represent "bulk" or non-qSIP samples.

## Usage

``` r
validate_gradient_position(gradient_position)
```

## Arguments

- gradient_position:

  (*string or strings*) Gradient position value or values

## Value

`NULL` if the gradient position values are valid, or a printed error
