# works correctly

    Code
      run_resampling(qsip_normal_strict_filtered, resamples = 100, with_seed = 21)
    Condition
      Warning:
      7 unlabeled and 0 labeled feature_ids had resampling failures.
      i Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect.
    Output
      <qsip_data>
      group: none
      feature_id count: 74 of 2030
      sample_id count: 284
      filtered: TRUE
      resampled: TRUE
      EAF: FALSE
      growth: FALSE

---

    Code
      run_resampling(qsip_normal_strict_filtered, resamples = 10)
    Message
      Using random seed. For consistency, you can use the with_seed argument
    Condition
      Warning:
      4 unlabeled and 0 labeled feature_ids had resampling failures.
      i Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect.
    Output
      <qsip_data>
      group: none
      feature_id count: 74 of 2030
      sample_id count: 284
      filtered: TRUE
      resampled: TRUE
      EAF: FALSE
      growth: FALSE

---

    Code
      run_resampling(qsip_normal_strict_filtered, resamples = 10, progress = FALSE)
    Message
      Using random seed. For consistency, you can use the with_seed argument
    Condition
      Warning:
      7 unlabeled and 0 labeled feature_ids had resampling failures.
      i Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect.
    Output
      <qsip_data>
      group: none
      feature_id count: 74 of 2030
      sample_id count: 284
      filtered: TRUE
      resampled: TRUE
      EAF: FALSE
      growth: FALSE

---

    Code
      run_resampling(qsip_normal_strict_filtered, resamples = 10, allow_failures = TRUE)
    Message
      Using random seed. For consistency, you can use the with_seed argument
    Condition
      Warning:
      7 unlabeled and 0 labeled feature_ids had resampling failures.
      i Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect.
    Output
      <qsip_data>
      group: none
      feature_id count: 74 of 2030
      sample_id count: 284
      filtered: TRUE
      resampled: TRUE
      EAF: FALSE
      growth: FALSE

