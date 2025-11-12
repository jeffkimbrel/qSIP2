# Check the validity of density values (internal)

Check the validity of density values (internal)

## Usage

``` r
validate_gradient_pos_density(df, low = 1.55, high = 1.8)
```

## Arguments

- df:

  (*dataframe*) A two-column dataframe with density positions and
  density values

- low:

  (*numeric, default: 1.55*) A low limit for valid density values

- high:

  (*numeric, default: 1.8*) A high limit for valid density values

## Value

Returns `NULL` if the density values are valid, or a printed error
