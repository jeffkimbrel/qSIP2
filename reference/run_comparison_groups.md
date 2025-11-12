# Run comparison groups

This function takes a dataframe of groups and parameters and runs an
entire qSIP2 EAF workflow for each group.

## Usage

``` r
run_comparison_groups(
  groups,
  qsip_data_object,
  allow_failures = NULL,
  seed = NULL,
  resamples = NULL
)
```

## Arguments

- groups:

  A dataframe containing group, unlabeled, labeled, and other optional
  columns

- qsip_data_object:

  A qsip_data object

- allow_failures:

  Whether to allow failures in the filtering step. Sets for all groups.

- seed:

  The seed for the resampling. Sets for all groups.

- resamples:

  The number of resamples to run. Sets for all groups.

## Details

The heart of this function is the group dataframe, which should contain
the following columns:

- group (required): a unique identifier for each group. This can be
  short, or a descriptive string describing the group

- unlabeled (required): a comma-separated list of `source_mat_id`s for
  the unlabeled isotope. Optionally, you can use terms such as
  "unlabeled" or "12C" to use all `source_mat_id`s with that isotope
  designation

- labeled (required): a comma-separated list of `source_mat_id`s for the
  labeled isotope

Additionally, other optional columns can be included in the dataframe to
set per group parameters:

- min_unlabeled_sources: the minimum number of unlabeled sources
  required for each fraction

- min_labeled_sources: the minimum number of labeled sources required
  for each fraction

- min_unlabeled_fractions: the minimum number of unlabeled fractions
  required for each source

- min_labeled_fractions: the minimum number of labeled fractions
  required for each source

- allow_failures: whether to allow failures in the filtering step

- resamples: the number of resamples to run

- seed: the seed for the resampling

The last three in the list can also be added as optional parameters, and
these will override any values in the groups dataframe.
