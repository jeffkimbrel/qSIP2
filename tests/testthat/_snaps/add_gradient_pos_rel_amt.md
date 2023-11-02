# works correctly

    Code
      add_gradient_pos_rel_amt(example_sample_df, amt = "avg_16S_g_soil",
        source_mat_id = "source")
    Output
      # A tibble: 284 x 7
         sample  source Fraction density_g_ml dna_conc avg_16S_g_soil
         <chr>   <chr>  <chr>           <dbl>    <dbl>          <dbl>
       1 149_F1  S149   1                1.78  0                4474.
       2 149_F2  S149   2                1.77  0                 987.
       3 149_F3  S149   3                1.77  0                4003.
       4 149_F4  S149   4                1.76  0                3960.
       5 149_F5  S149   5                1.75  0.00124          5726.
       6 149_F6  S149   6                1.75  0.0128           7566.
       7 149_F7  S149   7                1.74  0.776           30927.
       8 149_F8  S149   8                1.73  2.66            90143.
       9 149_F9  S149   9                1.73 16.1           1475519.
      10 149_F10 S149   10               1.73 22.2           6040362.
      # i 274 more rows
      # i 1 more variable: gradient_pos_rel_amt <dbl>

