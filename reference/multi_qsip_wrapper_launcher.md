# Internal function for launching multi_qsip_wrapper()

Internal function for launching multi_qsip_wrapper()

## Usage

``` r
multi_qsip_wrapper_launcher(
  group,
  name,
  qsip_data_object,
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

- group:

  A group dataframe

- name:

  A group name

- qsip_data_object:

  A qsip_data object

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
