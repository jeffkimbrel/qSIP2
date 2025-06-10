# qSIP2 0.20

* prepping for v1.0
* changed name of some functions so they match the larger naming scheme
  * `show_comparison_groups()` to `get_comparison_groups()`
  * `find_shared_ids()` to `get_shared_ids()`
  * `show_unshared_ids()` to `get_unshared_ids()`
* fixed isotope check bug (fixes #11)
  * added `valid_isotope_names` list as data to document the valid terms
* added `qsip_clean()` to reduce the size of large qSIP objects
* reworked the `is_x()` functions
* moved some packages involved in normalizations and corrections to suggests, rather than imports
* added `convert_negatives` argument to `qsip_sample_data()` to convert negative `gradient_pos_amt` values to zero
* `get_filter_results()` function gives tabular output of the filtering results
  * can set `type` to `counts` to get the retained features as counts (default)
  * or set to `feature_ids` to instead return nested vector of the feature IDs
* `get_filtered_feature_summary()` now returns a tibble with the feature IDs and their filtering fate
* Moved `propO` to `run_growth_calculations()`
* Added `N_light_it` to `summarize_growth_values()` output

# qSIP2 0.19

* Many functions for correcting WAD values (details coming soon)
* more documentation for spike-in control functions

# qSIP2 0.18

* Object type checks more consistent
* Updated `plot_sample_curves()` to have better options
* Added `correct_gradient_pos_density()` to adjust WAD values of sample data based on the gradient position
* Replace the *silent* argument with `get_all_by_isotope()` with *quiet* to fit better with other functions
* Added `qsip_object_size()` function to get the size of a `qsip_data` object and slots

# qSIP2 0.17

* New functions to work with JGI spike-ins
* Changed `plot_feature_occurrence()` to show all features by default
* Print methods for the 4 main objects to show summary information
* Added better error checking for sample data with duplicated sample names (#17)
* Deprecated the `plot_filter_gradient_position()` function and replaced with `plot_filter_results()`, and updated vignettes

# qSIP2 0.16

* Added new vignettes
    * `vignette("growth")`
    * `vignette("multiple_objects")`
    * `vignette("filtering")`
    * `vignette("EAF")`
* The previous vignettes were hastily written, but now they have been hastily re-written to capture the modern workflow of `qSIP2`
    * `vignette("qSIP_workflow")`
    * `vignette("source_data")`
    * `vignette("sample_data")`
    * `vignette("feature_data")`
    * `vignette("resampling")`
* Added light/dark switch to website documentation
* Added optional `title` argument to change the title of the plot to several functions
* Switched to using `testthat` fixtures
* Added `get_filtered_feature_summary()` to see the filtering fate of a specified `feature_id`
* Clean `check()` with no errors, warnings or notes. 
* Changed license to BSD-3
* `shared_y` argument (`TRUE`/`FALSE`) added to `plot_EAF_values()` to share the y-axis across all comparisons

# qSIP2 0.15

* Introducing functions to work with multiple qSIP2 objects at once
* Added `run_comparison_groups()` to launch multiple qSIP2 EAF workflows
* Updated `summarize_EAF_values()` and `plot_EAF_values()` functions to work with multiple qSIP2 objects. Plotting puts each group in it's own facet, and each facet is sorted individually by EAF values. The `top` argument also works for each group independently, but the `confidence` value is shared across all groups. 
* Less strict isotope validation to allow comparing mismatched labeled with unlabeled sources. For example, you can compare 18O labeled against 12C unlabeled sources.

# qSIP2 0.14

* Added grouping variable to `get_N_total_it()` to summarize by metadata variables
* Fixed bug where N_total_it should be just from unlabeled, not labeled + unlabeled
* Added linear growth model in addition to the existing exponential model. Exponential is still default.
* Renamed *unlabeled* and *labeled* to *N_light_it* and *N_heavy_it*, respectively
* Growth calculations more finalized
* when recalculating N_light_it, `qSIP2` now recalculates N_heavy_it to keep N_total_it constant

# qSIP2 0.13

* Beta functions for growth calculations
* Added `time` and `total_abundance` to `qsip_source_data()` as required arguments if you want to do the growth calculations
* Added `calculate_time_zero_abundance()` to summarize the time zero abundance for each feature
* Added `run_growth_calculations()` to calculate growth (r), birth (b) and death (d) rates from total abundances and EAF values
* Added `summarize_growth_values()` to summarize r, b and d rates
* Added `plot_growth_rates()` to visualize growth r, b and d rates
* Two different growth plot types based on either rates or N copies
* Both timepoints (time zero and time *t*) are now reported during growth calculations
* Other features that make comparing rates between timepoints that are not time zero easier
* Added a `group` argument to `run_feature_filter()` to embed a grouping name to the qsip object
* Ability to adjust the total abundance copies using a per-row volume adjustment in the source data
* Moved some Koch, 2018 equations to their own functions
* Resampling now calculates `r_net` and `N_total_it` values

# qSIP2 0.12

* Beginning work on KBase functionality
* Added additional columns to `summarize_EAF_values()` output and updated vignettes
* `plot_EAF_values()` function added
* Added new parameter (`gc_method`) to change the GC calculation method in `run_EAF_calculations()`. "MM" is still default. 
* Added ability to add/remove the `zero_line` from `plot_EAF_values()`

# qSIP2 0.11

* Fixed `run_resampling()` to not get confused when using sample names that are integers. 
* Fixed `example_source_df` and `example_sample_df` to remove the built in validation errors (missing `isotoplog` in the source data, and fractions as characters in the sample data). These dataframes are now valid objects for the package
* Added validation checks for existing standard names in dataframes. For example, if trying to use a data.frame with source data that already has a `source_mat_id` column, but you designate a different column as the `source_mat_id`, it will throw an error. This is to prevent column name collisions and potential unintended consequences. 
* Added internal function `validate_standard_names()` to check for existing standard names in dataframes.
* Added alpha function `plot_resampling_convergence()` to follow when the CoV of the resamplings converge to a stable value
* Added new `vignette("resampling")` for more details about the resampling procedure
* Fixed `run_EAF_calculations()` to work with `allow_failures` logic
* Add `plot_feature_resamplings()` to plot resampling results for a single or list of feature_ids
* Introduced ability to keep only successful resampling attempts, and discard failures. This keeps `run_resampling()` from failing if the sample count is low, but could result in feature_ids with less than the expected number of resamples. This is controlled by the `allow_failures` argument in `run_resampling()`. Two functions have been added to inspect the results of resampling: `get_resample_counts()` and `plot_successful_resamples()`.

# qSIP2 0.10

* Added `infer_source_data()` function to infer source data from a sample data data frame
* Update documentation of internal functions
* Fixed `validate_gradient_pos_density()` (and tests) to not fail with bulk data that has a gradient_position of `-1` (#8).
* Fixed `validate_isotopes()` to accept standard unfractionated terms like "bulk" or "time0" so they bypass isotope validation.
* `plot_sample_curves()` and `plot_source_wads()` have been updated to ignore unfractionated samples/sources
* Removed requirement for `gradient_pos_rel_amt` column in the imported sample dataframe. If you have one you can still pass the column name. If you don't, it will run the `add_gradient_pos_rel_amt()` silently using the `gradient_pos_amt` column, and provide a message that it is doing so.
* Updated `vignette("feature_data")`
* Documentation for `get_dataframe()`
* Started a NEWS.md file to keep track of changes
* Updated `plot_sample_curves()` and `plot_source_wads()` to use existing WAD values that were pre-calculated when making the `qsip_data` object. This means they now require a `qsip_data` object as input and no longer accept a sample or source object. The main workflow vignette was updated to reflect these changes.
* Renamed `data()` to `get_dataframe()` to match the naming scheme of other functions.
