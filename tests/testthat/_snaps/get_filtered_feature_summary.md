# doesn't error

    Code
      get_filtered_feature_summary(qsip_normal_strict_filtered, feature_id = "ASV_2")
    Output
      $fraction_filter_summary
      # A tibble: 11 x 6
         feature_id source_mat_id n_fractions tube_rel_abundance type    fraction_call
         <chr>      <chr>               <int>              <dbl> <chr>   <chr>        
       1 ASV_2      S149                   19             0.165  unlabe~ Fraction Pas~
       2 ASV_2      S150                   19             0.222  unlabe~ Fraction Pas~
       3 ASV_2      S151                   19             0.287  unlabe~ Fraction Pas~
       4 ASV_2      S152                   19             0.203  unlabe~ Fraction Pas~
       5 ASV_2      S161                   19             0.151  unlabe~ Fraction Pas~
       6 ASV_2      S162                   18             0.0391 unlabe~ Fraction Pas~
       7 ASV_2      S163                   19             0.0684 unlabe~ Fraction Pas~
       8 ASV_2      S164                   19             0.0321 unlabe~ Fraction Pas~
       9 ASV_2      S178                   18             0.110  labeled Fraction Pas~
      10 ASV_2      S179                   20             0.112  labeled Fraction Pas~
      11 ASV_2      S180                   19             0.114  labeled Fraction Pas~
      
      $source_filter_summary
      # A tibble: 2 x 5
        feature_id type      n_sources mean_tube_rel_abundance source_call  
        <chr>      <chr>         <int>                   <dbl> <chr>        
      1 ASV_2      labeled           3                   0.112 Source Passed
      2 ASV_2      unlabeled         8                   0.146 Source Passed
      
      $retained
      [1] TRUE
      

