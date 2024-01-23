# qSIP2 (development version)

* Added alpha function `plot_resampling_convergence` to follow when the CoV of the resamplings converge to a stable value

# qSIP2 0.11.1

* Fixed `run_EAF_calculations` to work with `allow_failures` logic
* Add `plot_feature_resamplings` to plot resampling results for a single or list of feature_ids

# qSIP2 0.11.0

* Introduced ability to keep only successful resampling attempts, and discard failures. This keeps `run_resampling()` from failing if the sample count is low, but could result in feature_ids with less than the expected number of resamples. This is controlled by the `keep_failures` argument in `run_resampling()`. Two functions have been added to inspect the results of resampling: `get_resample_counts()` and `plot_successful_resamples()`.

# qSIP2 0.10.6

* Added `infer_source_data()` function to infer source data from a sample data data frame
* Update documentation of internal functions
* Fixed `validate_gradient_pos_density()` (and tests) to not fail with bulk data that has a gradient_position of `-1 (#8).
* Fixed `validate_isotopes()` to accept standard unfractionated terms like "bulk" or "time0" so they bypass isotope validation.
* `plot_sample_curves()` and `plot_source_wads()` have been updated to ignore unfractionated samples/sources
* Removed requirement for `gradient_pos_rel_amt` column in the imported sample dataframe. If you have one you can still pass the column name. If you don't, it will run the `add_gradient_pos_rel_amt()` silently using the `gradient_pos_amt` column, and provide a message that it is doing so.
* Updated `vignette("feature_data")`
* Documentation for `get_dataframe()`

# qSIP2 0.10.5

* Started a NEWS.md file to keep track of changes
* Updated `plot_sample_curves()` and `plot_source_wads()` to use existing WAD values that were pre-calculated when making the `qsip_data` object. This means they now require a `qsip_data` object as input and no longer accept a sample or source object. The main workflow vignette was updated to reflect these changes.
* Renamed `data()` to `get_dataframe()` to match the naming scheme of other functions.
