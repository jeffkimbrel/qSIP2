# Calculate Cumulative Enrichment (CE) from qSIP object(s)

Calculate Cumulative Enrichment (CE) from qSIP object(s)

## Usage

``` r
calculate_ce(
  qsip_data_object,
  confidence = 0.95,
  isotope_label = c("labeled", "unlabeled", "both")
)
```

## Arguments

- qsip_data_object:

  A single qSIP object or list of qSIP objects

- confidence:

  Confidence level for EAF intervals (default 0.95)

- isotope_label:

  Which samples to calculate CE for: "labeled", "unlabeled", or "both"
  (default "labeled")

## Value

Data frame with CE calculations per sample
