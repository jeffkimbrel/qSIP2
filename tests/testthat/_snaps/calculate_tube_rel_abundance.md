# runs correctly

    Code
      calculate_tube_rel_abundance(example_source_object, example_sample_object,
        example_feature_object)
    Output
      # A tibble: 39,183 x 6
         feature_id sample_id source_mat_id tube_rel_abundance gradient_pos_density
         <chr>      <chr>     <chr>                      <dbl>                <dbl>
       1 ASV_1      149_F1    S149                  0.0000180                  1.78
       2 ASV_1      149_F2    S149                  0.00000411                 1.77
       3 ASV_1      149_F3    S149                  0.0000160                  1.77
       4 ASV_1      149_F4    S149                  0.0000167                  1.76
       5 ASV_1      149_F5    S149                  0.0000282                  1.75
       6 ASV_1      149_F6    S149                  0.0000310                  1.75
       7 ASV_1      149_F7    S149                  0.0000710                  1.74
       8 ASV_1      149_F8    S149                  0.000169                   1.73
       9 ASV_1      149_F9    S149                  0.000889                   1.73
      10 ASV_1      149_F10   S149                  0.00206                    1.73
      # i 39,173 more rows
      # i 1 more variable: gradient_pos_rel_amt <dbl>

