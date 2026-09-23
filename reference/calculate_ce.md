# Calculate Community Enrichment (CE) from qSIP object(s)

Calculate Community Enrichment (CE) from qSIP object(s)

## Usage

``` r
calculate_ce(
  qsip_data_object,
  confidence = 0.95,
  isotope_label = c("labeled", "unlabeled", "both"),
  enrichment_threshold = c("positive_eaf", "significant", "all")
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

- enrichment_threshold:

  Threshold for determining which features are enriched (default
  "positive_eaf"):

  - `"positive_eaf"` (recommended): Enrichment threshold at EAF ≥ 0. All
    features included, but negative EAF values are set to 0. This is the
    most defensible approach as it uses all available data while
    respecting the biological constraint that features cannot be
    depleted below natural abundance. Negative EAF values represent
    measurement noise and should not reduce community enrichment.

  - `"significant"`: Enrichment threshold at lower CI \> 0. Most
    conservative approach that only counts features with statistically
    significant positive enrichment. May underestimate community
    enrichment by excluding features with real but uncertain low
    enrichment.

  - `"all"`: No enrichment threshold. All features with raw observed_EAF
    values, including negatives. Matches the original per-capita
    calculation but allows biologically impossible negative enrichment
    to reduce CE estimates. Useful for backwards compatibility but not
    recommended for new analyses.

## Value

Data frame with CE calculations per sample containing:

- `group`: Group identifier from qSIP object

- `source_mat_id`: Sample identifier

- `n_features_total`: Total number of features in calculation

- `n_features_significant`: Number of features with lower CI \> 0

- `total_proportion`: Sum of tube_rel_abundance for all features
  (relative to whole community)

- `significant_proportion`: Sum of tube_rel_abundance for significant
  features only

- `ce`: Community enrichment - proportion of whole community biomass
  that is labeled (0 to total_proportion)

- `ce_norm`: Normalized community enrichment - CE renormalized to tested
  features (0 to 1). Calculated as ce/total_proportion. Treats tested
  features as 100% of the reference frame.

- `label_type`: "labeled" or "unlabeled"

- `isotope`: Isotope used (e.g., "18O", "13C")

- `isotopolog`: Isotopolog used (e.g., "water", "glucose")

- `messages`: List of warning/info messages from calculation

## Details

**Understanding CE vs CE_norm:**

`ce` (Community Enrichment) represents the proportion of the *entire
original community* (including features removed by upstream quality
filters) that has labeled biomass. When total_proportion \< 1.0 (e.g.,
0.8 means 20% of community abundance was removed by upstream quality
filters), CE is relative to the original 100%. CE values range from 0 to
total_proportion.

`ce_norm` (Normalized Community Enrichment) represents the same
enrichment as CE but renormalized to treat the *tested features only* as
100% of the community. ce_norm = ce / total_proportion. ce_norm values
range from 0 to 1.

When no upstream quality filtering occurs (total_proportion = 1.0), CE
and CE_norm are identical. When upstream filtering occurs, ce_norm \> ce
because ce_norm excludes those filtered features from the denominator.

**Example:** If CE = 0.048 and total_proportion = 0.8:

- CE interpretation: "4.8% of the whole community (including the 20%
  removed by upstream quality filters) has labeled biomass"

- CE_norm = 0.048 / 0.8 = 0.06: "6% of the tested features (80% of
  community) have labeled biomass on average"

**Important assumption:** CE and CE_norm calculations weight features by
their relative sequence abundance (tube_rel_abundance), which assumes
equal biomass per sequence read. In reality, features may differ in: (1)
cell/organism size and biomass, (2) marker gene copy number (e.g., 16S
rRNA gene copies per genome can range 1-15+), and (3) DNA extraction
efficiency. These factors cause relative abundance to diverge from
relative biomass, meaning two features with equal abundance may
contribute different amounts of labeled biomass. This is an inherent
limitation of amplicon-based qSIP that cannot be computationally
corrected. CE and CE_norm should be interpreted as abundance-weighted
metrics, not true biomass-weighted metrics.
