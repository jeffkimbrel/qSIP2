# works as expected

    Code
      get_growth_data(qsip_growth_rates)
    Output
      # A tibble: 31,805 x 13
         feature_id timepoint1 timepoint2 resample  N_total_i0 N_total_it N_light_it
         <chr>           <dbl>      <dbl> <chr>          <dbl>      <dbl>      <dbl>
       1 taxon_1             0         10 1        1595472105. 148586025. 135320570.
       2 taxon_1             0         10 2        1595472105. 148586025. 122548548.
       3 taxon_1             0         10 3        1595472105. 148586025. 139938298.
       4 taxon_1             0         10 4        1595472105. 148586025. 140910213.
       5 taxon_1             0         10 5        1595472105. 148586025. 133779727.
       6 taxon_1             0         10 6        1595472105. 148586025. 123018482.
       7 taxon_1             0         10 7        1595472105. 148586025. 123707452.
       8 taxon_1             0         10 8        1595472105. 148586025. 122989598.
       9 taxon_1             0         10 9        1595472105. 148586025. 117552963.
      10 taxon_1             0         10 10       1595472105. 148586025. 116707736.
      # i 31,795 more rows
      # i 6 more variables: N_heavy_it <dbl>, EAF <dbl>, r_net <dbl>, bi <dbl>,
      #   di <dbl>, ri <dbl>

