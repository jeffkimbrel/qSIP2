# Validate that a data.frame doesn't already contained standard column names (internal)

Currently used when instantiating one of the three "primal" objects. It
will give an error if the user tries to pass a non-standard column name,
but an existing column name already uses the standard name.

## Usage

``` r
validate_standard_names(data, name, type)
```

## Arguments

- data:

  A data.frame

- name:

  The column name selected by the user

- type:

  The type of data (source, sample, or feature)
