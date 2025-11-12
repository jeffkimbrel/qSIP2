# Calculate GC% from density/WAD

This function takes a `density` value and an optional `method` and
returns the predicted GC% content of a DNA sequence with that density.

## Usage

``` r
calculate_gc_from_density(density, method = "MM")
```

## Arguments

- density:

  (*numeric*) Density or WAD values

- method:

  (*string, default: MM*) The GC% calculation method

## Value

A vector of GC% values

## Details

The `method` parameter changes the formula from that provided by McHugh
& Morrissey (`MM`, unpublished) or Schildkraut (`S`, 1962).

This function corresponds to equation 5 from Hungate, 2015
