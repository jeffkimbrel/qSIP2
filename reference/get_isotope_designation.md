# Infer which isotope calculations to use (internal)

This uses the list of the source_mat_ids in a filtered qsip_data object
to get their isotope designation for two reasons, 1) to make sure they
are comparable (e.g. are all 12C/13C, 14N/15N or 16O/18O with no
mismatches), and 2) to return the best guess of the labeled isotope so
EAF calculations will proceed correctly.

## Usage

``` r
get_isotope_designation(
  qsip_data_object,
  unlabeled_source_mat_ids,
  labeled_source_mat_ids
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A `qsip_data` object

- unlabeled_source_mat_ids:

  (*character*) A vector of source_mat_ids that are unlabeled

- labeled_source_mat_ids:

  (*character*) A vector of source_mat_ids that are labeled

## Value

A single labeled isotope designation of 13C, 15N or 18O, and gives an
error if an inference cannot be made.

## Details

As of v0.15.2 it is now possible to have a mismatch. This is
particularly important with multiple isotope studies where you may want
to compare a 13C sample to a 12C sample, but also an 18O against the
same 12C sample.
