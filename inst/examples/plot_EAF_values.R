q <- example_qsip_object |>
  run_feature_filter(
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S178", "S179", "S180")
  ) |>
  run_resampling(resamples = 1000, progress = FALSE, allow_failures = TRUE) |>
  run_EAF_calculations()

# plot the top 25 without confidence intervals
qSIP2::plot_EAF_values(q, top = 25)

# add confidence intervals as a ribbon
qSIP2::plot_EAF_values(q, top = 25, error = "ribbon")

# increase the resample success stringency
qSIP2::plot_EAF_values(q, top = 25, error = "ribbon", success_ratio = 0.99)
