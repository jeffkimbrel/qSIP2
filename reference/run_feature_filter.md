# Filter features in a qSIP data object

Filters a feature in a feature table in a `qsip_data` object by presence
in a minimum number of fractions and source_mat_ids.

## Usage

``` r
run_feature_filter(
  qsip_data_object,
  group = NULL,
  unlabeled_source_mat_ids,
  labeled_source_mat_ids,
  min_unlabeled_sources = 2,
  min_labeled_sources = 2,
  min_unlabeled_fractions = 2,
  min_labeled_fractions = 2,
  quiet = FALSE
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) An object of `qsip_data` class

- group:

  (*string*) An optional name to assign to this filtered group

- unlabeled_source_mat_ids:

  (*string or strings(s)*) A list of the unlabeled source_mat_ids to
  filter on

- labeled_source_mat_ids:

  (*string or strings(s)*) A list of the labeled source_mat_ids to
  filter on

- min_unlabeled_sources:

  (*integer, default: 2*) Minimum number of unlabeled source_mat_ids a
  feature must be found in.

- min_labeled_sources:

  (*integer, default: 2*) Minimum number of labeled source_mat_ids a
  feature must be found in.

- min_unlabeled_fractions:

  (*integer, default: 2*) Minimum number of fractions a feature must be
  found in to be present in an unlabeled source_mat_id

- min_labeled_fractions:

  (*integer, default: 2*) Minimum number of fractions a feature must be
  found in to be present in an labeled source_mat_id

- quiet:

  (*logical, default: FALSE*) Suppress messages

## Value

An updated `qsip_data` object with a filtered feature dataframe in the
`@filtered_feature_data` slot and intermediate data in the
`@filter_results` slot for plotting.

## Details

Filtering is first done on the fractions, and then on the
source_mat_ids. For example, take a feature that is found in three
source_mat_ids in 3, 5 and 9 fractions. If you set `min_fractions = 5`
and `min_sources = 3` then this feature will not survive the filtering
because although it is found in three source_mat_ids, one of them is
less then the minimum fraction count and would therefore be considered
not found in that source_mat_id.

The feature table is filtered to retain only the feature_ids passing the
filter, and to keep only the sample_ids that correspond to the given
source_mat_ids. This filtered table is stored in the
`@filtered_feature_data` slot, and the values in this table are not the
raw initial values but are the relative abundances per fraction per
tube. These values are originally calculated during `qsip_data` object
creation and the values for all features are stored in the
`@tube_rel_abundance` slot.
