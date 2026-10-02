# Bar plot showing mean filtering efficiency by treatment group

Visualizes filtering efficiency as bar plots showing mean feature and
abundance retention across sources within each treatment group. Shows
three categories:

- **Unlabeled** (blue): Features that passed filters in unlabeled
  samples

- **Labeled** (red): Features that passed filters in labeled samples

- **Retained** (purple): Features in the intersection (passed BOTH
  labeled and unlabeled)

## Usage

``` r
plot_filter_means(qsip_data_object, use_counts = FALSE)
```

## Arguments

- qsip_data_object:

  A filtered qsip_data object (or list)

- use_counts:

  If TRUE, plot absolute feature counts; if FALSE (default), plot
  percentages

## Value

A ggplot2 object

## Details

**Interpreting the bars:**

- **Taller bars** = Higher retention (more features/abundance retained)

- **Retained (purple)** bars are always at or below both labeled and
  unlabeled bars because the intersection cannot exceed either
  individual set

- **Error bars** show standard deviation across sources within the
  treatment group

- **Large gap** between labeled/unlabeled and retained = Poor filtering
  consistency (many features unique to one isotope)

- **Small gap** = Good filtering consistency (most features that passed
  one isotope also passed the other)

- **Asymmetric bars**: If unlabeled bar is much taller than labeled (or
  vice versa), one isotope had less stringent filtering and passed more
  features

See
[`plot_filter_efficiency`](https://jeffkimbrel.github.io/qSIP2/reference/plot_filter_efficiency.md)
for per-source detail and arrow-based visualization of filtering
consistency.
