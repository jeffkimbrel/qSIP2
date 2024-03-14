# works correctly

    Code
      add_isotopolog_label(example_source_df, isotope = "Isotope")
    Warning <lifecycle_warning_deprecated>
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(isotope)
      
        # Now:
        data %>% select(all_of(isotope))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
    Output
      # A tibble: 15 x 7
         source total_copies_per_g total_dna isotope isotopolog_label     Moisture
         <chr>               <dbl>     <dbl> <chr>   <chr>                <chr>   
       1 S149            34838665.      74.5 13C     natural abundance    Normal  
       2 S150            53528072.     109.  13C     natural abundance    Normal  
       3 S151            95774992.     182.  13C     natural abundance    Normal  
       4 S152             9126192.      23.7 13C     natural abundance    Normal  
       5 S161            41744046.      67.6 13C     natural abundance    Drought 
       6 S162            49402713.      94.2 13C     natural abundance    Drought 
       7 S163            47777726.      87.8 13C     natural abundance    Drought 
       8 S164            48734282.      76.0 13C     natural abundance    Drought 
       9 S178            62964478.      73.9 13C     isotopically labeled Normal  
      10 S179            49475460.      68.7 13C     isotopically labeled Normal  
      11 S180            51720787.      81.4 13C     isotopically labeled Normal  
      12 S200            59426155.      71.2 13C     isotopically labeled Drought 
      13 S201            56379702.      73.8 13C     isotopically labeled Drought 
      14 S202            42562198.     108.  13C     isotopically labeled Drought 
      15 S203            49914369.      80.5 13C     isotopically labeled Drought 
      # i 1 more variable: isotopolog <chr>

