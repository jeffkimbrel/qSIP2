# dimensions of returned data are correct

    Code
      calculate_wads(example_qsip_object@tube_rel_abundance)
    Output
      $wads
      # A tibble: 9,282 x 4
         feature_id source_mat_id   WAD n_fractions
         <chr>      <chr>         <dbl>       <int>
       1 ASV_1000   S203           1.71           1
       2 ASV_1001   S149           1.75           1
       3 ASV_1001   S164           1.76           1
       4 ASV_1001   S179           1.69           1
       5 ASV_1001   S180           1.73           1
       6 ASV_1002   S179           1.69           1
       7 ASV_1004   S151           1.68           1
       8 ASV_1004   S162           1.68           1
       9 ASV_1004   S163           1.73           1
      10 ASV_1004   S179           1.70           1
      # i 9,272 more rows
      
      $fraction_counts
      # A tibble: 30,450 x 3
         feature_id source_mat_id n_fractions
         <chr>      <chr>               <int>
       1 ASV_1      S203                   19
       2 ASV_1      S149                   19
       3 ASV_1      S164                   19
       4 ASV_1      S179                   16
       5 ASV_1      S180                   19
       6 ASV_1      S151                   19
       7 ASV_1      S162                   18
       8 ASV_1      S163                   19
       9 ASV_1      S201                   15
      10 ASV_1      S150                   19
      # i 30,440 more rows
      

