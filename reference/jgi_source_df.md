# Make a source dataframe from JGI proposal file

Make a source dataframe from JGI proposal file

## Usage

``` r
jgi_source_df(file_path, skip = 27)
```

## Arguments

- file_path:

  A file path to a JGI proposal file

- skip:

  The number of lines to skip in the proposal file

## Value

A dataframe suitable for `qSIP2` workflows

## See also

Other "spike-ins":
[`fit_regression_model()`](https://jeffkimbrel.github.io/qSIP2/reference/fit_regression_model.md),
[`get_normalized_controls()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_controls.md),
[`get_normalized_features()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_features.md),
[`jgi_feature_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_feature_df.md),
[`jgi_normalize_features()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_normalize_features.md),
[`jgi_sample_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_sample_df.md)
