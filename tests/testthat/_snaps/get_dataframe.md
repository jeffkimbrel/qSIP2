# correct dataframes are returned

    Code
      get_dataframe(example_feature_object)
    Output
      # A tibble: 2,030 x 285
         feature_id `149_F1` `149_F2` `149_F3` `149_F4` `149_F5` `149_F6` `149_F7`
         <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
       1 ASV_1          1245      376      582     1258      692     1482      812
       2 ASV_2          1471      569      830     1373      737     1777     2086
       3 ASV_3           342      152      211      389      218      461      304
       4 ASV_4           288      119      161      294      157      381      242
       5 ASV_5           317      108       95      292      164      310      216
       6 ASV_6           201       73      130      250      112      220      142
       7 ASV_7           223       82      102      243      136      230      227
       8 ASV_8           131       47       52      133       90      152      131
       9 ASV_9           159       74       89      143       83      191      127
      10 ASV_10           96       58       73      115       64      104       90
      # i 2,020 more rows
      # i 277 more variables: `149_F8` <dbl>, `149_F9` <dbl>, `149_F10` <dbl>,
      #   `149_F11` <dbl>, `149_F12` <dbl>, `149_F13` <dbl>, `149_F14` <dbl>,
      #   `149_F15` <dbl>, `149_F16` <dbl>, `149_F17` <dbl>, `149_F18` <dbl>,
      #   `149_F19` <dbl>, `150_F1` <dbl>, `150_F2` <dbl>, `150_F3` <dbl>,
      #   `150_F4` <dbl>, `150_F5` <dbl>, `150_F6` <dbl>, `150_F7` <dbl>,
      #   `150_F8` <dbl>, `150_F9` <dbl>, `150_F10` <dbl>, `150_F11` <dbl>, ...

---

    Code
      get_dataframe(example_sample_object)
    Output
      # A tibble: 284 x 7
         sample_id source_mat_id gradient_position gradient_pos_density
         <chr>     <chr>                     <int>                <dbl>
       1 149_F1    S149                          1                 1.78
       2 149_F2    S149                          2                 1.77
       3 149_F3    S149                          3                 1.77
       4 149_F4    S149                          4                 1.76
       5 149_F5    S149                          5                 1.75
       6 149_F6    S149                          6                 1.75
       7 149_F7    S149                          7                 1.74
       8 149_F8    S149                          8                 1.73
       9 149_F9    S149                          9                 1.73
      10 149_F10   S149                         10                 1.73
      # i 274 more rows
      # i 3 more variables: gradient_pos_amt <dbl>, gradient_pos_rel_amt <dbl>,
      #   dna_conc <dbl>

---

    Code
      get_dataframe(example_source_object)
    Output
      # A tibble: 15 x 6
         isotope isotopolog source_mat_id total_copies_per_g total_dna Moisture
         <chr>   <chr>      <chr>                      <dbl>     <dbl> <chr>   
       1 12C     glucose    S149                   34838665.      74.5 Normal  
       2 12C     glucose    S150                   53528072.     109.  Normal  
       3 12C     glucose    S151                   95774992.     182.  Normal  
       4 12C     glucose    S152                    9126192.      23.7 Normal  
       5 12C     glucose    S161                   41744046.      67.6 Drought 
       6 12C     glucose    S162                   49402713.      94.2 Drought 
       7 12C     glucose    S163                   47777726.      87.8 Drought 
       8 12C     glucose    S164                   48734282.      76.0 Drought 
       9 13C     glucose    S178                   62964478.      73.9 Normal  
      10 13C     glucose    S179                   49475460.      68.7 Normal  
      11 13C     glucose    S180                   51720787.      81.4 Normal  
      12 13C     glucose    S200                   59426155.      71.2 Drought 
      13 13C     glucose    S201                   56379702.      73.8 Drought 
      14 13C     glucose    S202                   42562198.     108.  Drought 
      15 13C     glucose    S203                   49914369.      80.5 Drought 

---

    Code
      get_dataframe(example_qsip_object, type = "feature")
    Output
      # A tibble: 2,030 x 285
         feature_id `149_F1` `149_F2` `149_F3` `149_F4` `149_F5` `149_F6` `149_F7`
         <chr>         <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
       1 ASV_1          1245      376      582     1258      692     1482      812
       2 ASV_2          1471      569      830     1373      737     1777     2086
       3 ASV_3           342      152      211      389      218      461      304
       4 ASV_4           288      119      161      294      157      381      242
       5 ASV_5           317      108       95      292      164      310      216
       6 ASV_6           201       73      130      250      112      220      142
       7 ASV_7           223       82      102      243      136      230      227
       8 ASV_8           131       47       52      133       90      152      131
       9 ASV_9           159       74       89      143       83      191      127
      10 ASV_10           96       58       73      115       64      104       90
      # i 2,020 more rows
      # i 277 more variables: `149_F8` <dbl>, `149_F9` <dbl>, `149_F10` <dbl>,
      #   `149_F11` <dbl>, `149_F12` <dbl>, `149_F13` <dbl>, `149_F14` <dbl>,
      #   `149_F15` <dbl>, `149_F16` <dbl>, `149_F17` <dbl>, `149_F18` <dbl>,
      #   `149_F19` <dbl>, `150_F1` <dbl>, `150_F2` <dbl>, `150_F3` <dbl>,
      #   `150_F4` <dbl>, `150_F5` <dbl>, `150_F6` <dbl>, `150_F7` <dbl>,
      #   `150_F8` <dbl>, `150_F9` <dbl>, `150_F10` <dbl>, `150_F11` <dbl>, ...

---

    Code
      get_dataframe(example_qsip_object, type = "sample")
    Output
      # A tibble: 284 x 7
         sample_id source_mat_id gradient_position gradient_pos_density
         <chr>     <chr>                     <int>                <dbl>
       1 149_F1    S149                          1                 1.78
       2 149_F2    S149                          2                 1.77
       3 149_F3    S149                          3                 1.77
       4 149_F4    S149                          4                 1.76
       5 149_F5    S149                          5                 1.75
       6 149_F6    S149                          6                 1.75
       7 149_F7    S149                          7                 1.74
       8 149_F8    S149                          8                 1.73
       9 149_F9    S149                          9                 1.73
      10 149_F10   S149                         10                 1.73
      # i 274 more rows
      # i 3 more variables: gradient_pos_amt <dbl>, gradient_pos_rel_amt <dbl>,
      #   dna_conc <dbl>

---

    Code
      get_dataframe(example_qsip_object, type = "source")
    Output
      # A tibble: 15 x 6
         isotope isotopolog source_mat_id total_copies_per_g total_dna Moisture
         <chr>   <chr>      <chr>                      <dbl>     <dbl> <chr>   
       1 12C     glucose    S149                   34838665.      74.5 Normal  
       2 12C     glucose    S150                   53528072.     109.  Normal  
       3 12C     glucose    S151                   95774992.     182.  Normal  
       4 12C     glucose    S152                    9126192.      23.7 Normal  
       5 12C     glucose    S161                   41744046.      67.6 Drought 
       6 12C     glucose    S162                   49402713.      94.2 Drought 
       7 12C     glucose    S163                   47777726.      87.8 Drought 
       8 12C     glucose    S164                   48734282.      76.0 Drought 
       9 13C     glucose    S178                   62964478.      73.9 Normal  
      10 13C     glucose    S179                   49475460.      68.7 Normal  
      11 13C     glucose    S180                   51720787.      81.4 Normal  
      12 13C     glucose    S200                   59426155.      71.2 Drought 
      13 13C     glucose    S201                   56379702.      73.8 Drought 
      14 13C     glucose    S202                   42562198.     108.  Drought 
      15 13C     glucose    S203                   49914369.      80.5 Drought 

