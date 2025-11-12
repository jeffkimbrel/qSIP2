# Fit regression model to spike-in control data

This is an internal function that does the actual fitting of the
regression model to the spike-in control data, and is called by
[`jgi_normalize_features()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_normalize_features.md)
via a [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)
call after nesting the data.

## Usage

``` r
fit_regression_model(control_data, method = "lm")
```

## Arguments

- control_data:

  A data frame containing the spike-in control data

- method:

  The regression engine to use. Choices are "lm" or "glm"

## Value

a workflow object

## Details

The output is a workflow object that can be used for fitting with
`broom::augment(parsnip::extract_fit_engine(x))`.

## See also

Other "spike-ins":
[`get_normalized_controls()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_controls.md),
[`get_normalized_features()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_features.md),
[`jgi_feature_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_feature_df.md),
[`jgi_normalize_features()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_normalize_features.md),
[`jgi_sample_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_sample_df.md),
[`jgi_source_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_source_df.md)
