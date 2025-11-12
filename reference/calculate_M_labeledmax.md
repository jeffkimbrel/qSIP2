# Calculate molecular weight of the labeled feature (internal)

This function corresponds to equation 7 from Hungate, 2015

## Usage

``` r
calculate_M_labeledmax(M, atom_count, isotope)
```

## Arguments

- M:

  (*numeric*) Molecular weight of the unlabeled feature

- atom_count:

  (*numeric*) The count of the relevant atoms (C, N or O)

- isotope:

  (*string*) The heavy isotope determining which calculation to run.
  Needs to be 13C, 15N or 18O

## Value

`M_labeledmax` is the theoretical maximum molecular weight the labeled
feature could be
