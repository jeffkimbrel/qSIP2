# works with single qsip_source_data object

    Code
      get_object_summary(example_source_object)
    Output
      # A tibble: 1 x 2
        group source_material_id_count
        <chr> <chr>                   
      1 <NA>  15                      

# works with single qsip_sample_data object

    Code
      get_object_summary(example_sample_object)
    Output
      # A tibble: 1 x 3
        group source_material_id_count sample_id_count
        <chr> <chr>                    <chr>          
      1 <NA>  15                       284            

# works with single qsip_feature_data object

    Code
      get_object_summary(example_feature_object)
    Output
      # A tibble: 1 x 5
        group feature_id_count sample_id_count data_type taxonomy
        <chr> <chr>            <chr>           <chr>     <chr>   
      1 <NA>  2030             284             counts    FALSE   

# works with single qsip_data object

    Code
      get_object_summary(example_qsip_object)
    Output
      # A tibble: 1 x 9
        group feature_count_original filtered feature_count_filtered
        <chr> <chr>                  <chr>    <chr>                 
      1 none  2030                   FALSE    <NA>                  
      # i 5 more variables: unlabeled_source_count <chr>, labeled_source_count <chr>,
      #   resampled <chr>, eaf <chr>, growth <chr>

# works with single qsip_data object with a group

    Code
      get_object_summary(qsip_normal_strict_filtered)
    Output
      # A tibble: 1 x 9
        group feature_count_original filtered feature_count_filtered
        <chr> <chr>                  <chr>    <chr>                 
      1 none  2030                   TRUE     74                    
      # i 5 more variables: unlabeled_source_count <chr>, labeled_source_count <chr>,
      #   resampled <chr>, eaf <chr>, growth <chr>

# works with a named list of qsip_data objects

    Code
      get_object_summary(li)
    Condition
      Warning:
      Duplicate group names detected: "none"
      i Multiple list elements have the same internal group name.
      > Check which rows correspond to which list elements.
    Output
      # A tibble: 2 x 9
        group feature_count_original filtered feature_count_filtered
        <chr> <chr>                  <chr>    <chr>                 
      1 none  2030                   FALSE    <NA>                  
      2 none  2030                   FALSE    <NA>                  
      # i 5 more variables: unlabeled_source_count <chr>, labeled_source_count <chr>,
      #   resampled <chr>, eaf <chr>, growth <chr>

# works with source_format = 'ids' for single qsip_data object

    Code
      result
    Output
      # A tibble: 1 x 9
        group feature_count_original filtered feature_count_filtered
        <chr> <chr>                  <chr>    <chr>                 
      1 none  2030                   TRUE     74                    
      # i 5 more variables: unlabeled_source_ids <list>, labeled_source_ids <list>,
      #   resampled <chr>, eaf <chr>, growth <chr>

# works with source_format = 'ids' for list of qsip_data objects

    Code
      result
    Output
      # A tibble: 2 x 9
        group feature_count_original filtered feature_count_filtered
        <chr> <chr>                  <chr>    <chr>                 
      1 none  2030                   TRUE     74                    
      2 none  2030                   TRUE     74                    
      # i 5 more variables: unlabeled_source_ids <list>, labeled_source_ids <list>,
      #   resampled <chr>, eaf <chr>, growth <chr>

