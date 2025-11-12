# Wrapper for running the qSIP EAF workflow

Wrapper for running the qSIP EAF workflow

## Usage

``` r
multi_qsip_wrapper(
  qsip_data_object,
  group = NULL,
  unlabeled_source_mat_ids,
  labeled_source_mat_ids,
  min_unlabeled_sources = 2,
  min_labeled_sources = 2,
  min_unlabeled_fractions = 2,
  min_labeled_fractions = 2,
  allow_failures = FALSE,
  resamples = 1000,
  seed = NULL
)
```

## Arguments

- qsip_data_object:

  A qsip_data object

- group:

  A group name

- unlabeled_source_mat_ids:

  A vector of source_mat_ids for the unlabeled isotope

- labeled_source_mat_ids:

  A vector of source_mat_ids for the labeled isotope

- min_unlabeled_sources:

  The minimum number of sources for the unlabeled isotope

- min_labeled_sources:

  The minimum number of sources for the labeled isotope

- min_unlabeled_fractions:

  The minimum number of fractions for the unlabeled isotope

- min_labeled_fractions:

  The minimum number of fractions for the labeled isotope

- allow_failures:

  Whether to allow failures in the filtering step

- resamples:

  The number of resamples to run

- seed:

  The seed for the resampling
