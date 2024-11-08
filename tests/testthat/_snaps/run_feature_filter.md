# works properly

    Code
      run_feature_filter(example_qsip_object, unlabeled_source_mat_ids = get_all_by_isotope(
        example_qsip_object, "12C"), labeled_source_mat_ids = c("S178", "S179",
        "S180"))
    Message
      There are initially 2030 unique feature_ids
      1705 of these have abundance in at least one fraction of one source_mat_id
      =+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
      Filtering feature_ids by fraction...
      1519 unlabeled and 1417 labeled feature_ids were found in zero fractions in at least one source_mat_id
      1210 unlabeled and 584 labeled feature_ids were found in too few fractions in at least one source_mat_id
      780 unlabeled and 497 labeled feature_ids passed the fraction filter
      In total, 870 unique feature_ids passed the fraction filtering requirements...
      =+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
      Filtering feature_ids by source...
      90 unlabeled and 373 labeled feature_ids failed the source filter because they were found in zero sources
      245 unlabeled and 189 labeled feature_ids failed the source filter because they were found in too few sources
      535 unlabeled and 308 labeled feature_ids passed the source filter
      =+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+=+
      In total, 257 unique feature_ids passed all fraction and source filtering requirements
    Output
      <qsip_data>
      group: none
      feature_id count: 257 of 2030
      sample_id count: 284
      filtered: TRUE
      resampled: FALSE
      EAF: FALSE
      growth: FALSE

