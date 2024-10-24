# works as expected

    Code
      run_growth_calculations(qsip_growth_EAF, N_total_it = example_qsip_growth_t0)
    Condition
      Warning:
      32 calculated values of unlabeled samples are negative. These values have been filtered out and added to @growth$negative_unlabeled
      Warning:
      3968 resamplings have a negative EAF value or calculated labeled copy numbers less than 0. These values have been filtered out and added to @growth$negative_labeled
    Output
      <qSIP2::qsip_data>
      group: Day 10
      feature_id count: 358 of 364
      sample_id count: 106
      filtered: TRUE
      resampled: TRUE
      EAF: TRUE
      growth: TRUE

