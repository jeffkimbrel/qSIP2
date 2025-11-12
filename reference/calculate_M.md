# Calculate molecular weight of the labeled feature (internal)

The GC value given to this equation is usually calculated from the
density value, not derived from the sequence itself.

## Usage

``` r
calculate_M(G)
```

## Arguments

- G:

  (*numeric*) GC content of a feature, ranges from 0-1.

## Value

`M` is the molecular weight of a sequence with `G` GC content

## Details

This function corresponds to equation 6 from Hungate, 2015.
