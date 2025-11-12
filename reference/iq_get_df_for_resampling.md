# Make nested dataframe for gpd adjustment sampling

Make nested dataframe for gpd adjustment sampling

## Usage

``` r
iq_get_df_for_resampling(
  qsip_data_object,
  wad_reference,
  fraction_cutoff = 5,
  source_cutoff = 3
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object

- wad_reference:

  (*tibble*) A tibble of WAD reference values

- fraction_cutoff:

  (*numeric, default: 5*) The minimum number of fractions a feature must
  be found in to be considered

- source_cutoff:

  (*numeric, default: 3*) The minimum number of sources a feature must
  be found in to be considered
