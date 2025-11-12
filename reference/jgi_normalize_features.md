# Normalize feature coverages according to spike-in controls with a known ratio

Given a dataframe of features and their coverages, this function will
normalize the coverages of the features according to the coverages of
the spike-in controls. Both the control and experimental features should
be mixed into the same dataframe, and the control features will be
identified based on internal data.

## Usage

``` r
jgi_normalize_features(features, samples, method = "lm")
```

## Arguments

- features:

  A dataframe with feature_id and coverage columns

- samples:

  A dataframe with sample_id, MIX, and sequins_pg columns

- method:

  The regression engine to use. Choices are "lm" or "glm"

## Value

A tibble with nested dataframes for each sample, containing the original
coverage data, the normalized coverage data, and the model information

## Details

Using the control features and their known stoichiometry, a regression
model is built converting the coverage to a picogram (pg) amount of DNA.
This model is run against the experimental data to convert their
coverages into pg amounts of DNA.

This function requires the samples dataframe because it contains the MIX
and sequins_pg columns, which are needed to normalize the features.

The output is a sort of intermediate file where a user can get the
normalized experimental data with the
[`get_normalized_features()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_features.md)
function, which is the input to the main `qSIP2` workflow. Or, the user
can get the normalized control data with the
[`get_normalized_controls()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_controls.md)
function in order to see how well the model fits the spike-in controls,
and to assess how well each individual spike-in sequences is behaving.

## See also

Other "spike-ins":
[`fit_regression_model()`](https://jeffkimbrel.github.io/qSIP2/reference/fit_regression_model.md),
[`get_normalized_controls()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_controls.md),
[`get_normalized_features()`](https://jeffkimbrel.github.io/qSIP2/reference/get_normalized_features.md),
[`jgi_feature_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_feature_df.md),
[`jgi_sample_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_sample_df.md),
[`jgi_source_df()`](https://jeffkimbrel.github.io/qSIP2/reference/jgi_source_df.md)
