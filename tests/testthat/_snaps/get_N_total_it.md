# works with correct object

    Code
      get_N_total_it(example_qsip_growth_object, t = 0)
    Condition
      Warning:
      1 feature_ids have zero abundance at time 0:
      Warning:
      taxon_194
    Output
      # A tibble: 364 x 3
         feature_id  N_total_i0 timepoint1
         <chr>            <dbl>      <dbl>
       1 taxon_1    1595472105.          0
       2 taxon_2      64576684.          0
       3 taxon_3       4488930.          0
       4 taxon_4       2494463.          0
       5 taxon_5       9849881.          0
       6 taxon_6     697760597.          0
       7 taxon_7       4702904.          0
       8 taxon_8     224366766.          0
       9 taxon_9       1386539.          0
      10 taxon_10      2793486.          0
      # i 354 more rows

# works with grouping

    Code
      get_N_total_it(example_qsip_growth_object, t = 0, group = "isotopolog")
    Condition
      Warning:
      1 feature_ids have zero abundance at time 0:
      Warning:
      taxon_194
    Output
      # A tibble: 364 x 4
         feature_id isotopolog  N_total_i0 timepoint1
         <chr>      <chr>            <dbl>      <dbl>
       1 taxon_1    water      1595472105.          0
       2 taxon_2    water        64576684.          0
       3 taxon_3    water         4488930.          0
       4 taxon_4    water         2494463.          0
       5 taxon_5    water         9849881.          0
       6 taxon_6    water       697760597.          0
       7 taxon_7    water         4702904.          0
       8 taxon_8    water       224366766.          0
       9 taxon_9    water         1386539.          0
      10 taxon_10   water         2793486.          0
      # i 354 more rows

