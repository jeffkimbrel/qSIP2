# qSIP2 (development version)

* Added `infer_source_data()` function to infer source data from a sample data data frame
* Update documentation of internal functions
* Fixed `validate_gradient_pos_density()` (and tests) to not fail with bulk data that has a gradient_position of `-1 (#8).
* Fixed `validate_isotopes()` to accept standard unfractionated terms like "bulk" or "time0" so they bypass isotope validation.
* `plot_sample_curves()` and `plot_source_wads()` have been updated to ignore unfractionated samples/sources

# qSIP2 0.10.5

* Started a NEWS.md file to keep track of changes
* Updated `plot_sample_curves()` and `plot_source_wads()` to use existing WAD values that were pre-calculated when making the `qsip_data` object. This means they now require a `qsip_data` object as input and no longer accept a sample or source object. The main workflow vignette was updated to reflect these changes.
* Renamed `data()` to `get_dataframe()` to match the naming scheme of other functions.
