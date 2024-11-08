# works correctly

    Code
      run_resampling(qsip_normal_strict_filtered, resamples = 100, with_seed = 21)
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
      NA unlabeled and NA labeled feature_ids had resampling failures. Run `get_resample_counts()` or `plot_successful_resamples()` on your <qsip_data> object to inspect.
    Output
      <qsip_data>
      group: none
      feature_id count: 74 of 2030
      sample_id count: 284
      filtered: TRUE
      resampled: TRUE
      EAF: FALSE
      growth: FALSE

