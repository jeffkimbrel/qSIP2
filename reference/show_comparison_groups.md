# Show comparison groups (deprecated)

**\[deprecated\]**

## Usage

``` r
show_comparison_groups(
  source_data = NULL,
  group = NULL,
  isotope = "isotope",
  source_mat_id = "source_mat_id"
)
```

## Arguments

- source_data:

  (*dataframe, qsip_source_data or qsip_data*) Sample metadata

- group:

  (*string*) Treatment value or values

- isotope:

  (*string, default: isotope*) Column name with isotope data

- source_mat_id:

  (*string, default: source_mat_id*) Column name with source_mat_id

## Details

Generates a table of ids grouped in columns by isotope, and in rows by
the given treatments.
