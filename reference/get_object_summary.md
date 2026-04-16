# Summarize a qSIP S7 object or named list of qSIP S7 objects

Summarize a qSIP S7 object or named list of qSIP S7 objects

## Usage

``` r
get_object_summary(x)
```

## Arguments

- x:

  A qSIP S7 object, or a named list of qSIP S7 objects of the same type.

## Value

For a single object, a tibble with `name` and `value` columns. For a
named list, a tibble with a `name` column and one column per list
element, named after the list elements.
