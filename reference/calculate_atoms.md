# Get the number of relevant atoms per nucleotide (internal)

Carbon and nitrogen atoms varies with GC content, but oxygen content is
constant for DNA regardless of GC content.

## Usage

``` r
calculate_atoms(G, isotope)
```

## Arguments

- G:

  (*numeric*) GC percentage

- isotope:

  (*string*) The isotope to use for calculations... either 13C, 15N or
  18O

## Value

(*numeric*) The number of atoms per nucleotide
