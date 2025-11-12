# Get unlabeled features for WAD reference (internal)

Internal function used by the "correct_gpd\_" functions to grab
unlabeled features that fit a certain prevalence criteria, and return
their mean and median WAD values, as well as their standard deviation,
coefficient of variation, and number of sources/fractions found in.

## Usage

``` r
iq_get_wad_reference(
  qsip_data_object,
  fraction_cutoff = 5,
  source_cutoff = 3,
  quiet = F
)
```

## Arguments

- qsip_data_object:

  (*qsip_data*) A qsip data object

- fraction_cutoff:

  (*numeric, default: 5*) The minimum number of fractions a feature must
  be found in to be considered

- source_cutoff:

  (*numeric, default: 3*) The minimum number of sources a feature must
  be found in to be considered

- quiet:

  (*logical, default: FALSE*) Whether to print a message about the
  number of features found

## Value

A tibble
