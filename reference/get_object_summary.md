# Summarize a qSIP S7 object or named list of qSIP S7 objects

Summarize a qSIP S7 object or named list of qSIP S7 objects

## Usage

``` r
get_object_summary(x, source_format = c("count", "ids"))
```

## Arguments

- x:

  A qSIP S7 object, or a named list of qSIP S7 objects of the same type.

- source_format:

  (*string, default: count*) Format for source material IDs. Either
  "count" to show the count of source IDs, or "ids" to show the actual
  vector of source IDs as list columns.

## Value

A tibble with a `group` column and columns for each metric. For single
objects, there is one row. For named lists, there is one row per list
element with the group name (or list element name) in the `group`
column. Metrics include `feature_count_original`,
`feature_count_filtered`, `unlabeled_source_count`/
`unlabeled_source_ids`, `labeled_source_count`/`labeled_source_ids`
(depending on `source_format`), `filtered`, `resampled`, `eaf`, and
`growth`.
