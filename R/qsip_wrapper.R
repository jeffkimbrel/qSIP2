#'
#'


run_qsip_wrapper = function(qsip_data_object,
                            group = NULL,
                            unlabeled_source_mat_ids,
                            labeled_source_mat_ids,
                            min_unlabeled_sources = 2,
                            min_labeled_sources = 2,
                            min_unlabeled_fractions = 2,
                            min_labeled_fractions = 2
) {



  run_feature_filter(qsip_data_object = qsip_data_object,
                     group = group,
                     unlabeled_source_mat_ids = unlabeled_source_mat_ids,
                     labeled_source_mat_ids = labeled_source_mat_ids,
                     min_unlabeled_sources = min_unlabeled_sources,
                     min_labeled_sources = min_labeled_sources,
                     min_unlabeled_fractions = min_unlabeled_fractions,
                     min_labeled_fractions = min_labeled_fractions,
                     quiet = TRUE
  ) |>
    run_resampling(allow_failures = TRUE,
                   resamples = 1000,
                   with_seed = 17,
                   progress = FALSE,
                   quiet = TRUE) |>
    run_EAF_calculations()

}
