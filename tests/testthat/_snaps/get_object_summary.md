# works with single qsip_source_data object

    Code
      get_object_summary(example_source_object)
    Output
      # A tibble: 1 x 2
        metric                   value
        <chr>                    <chr>
      1 source_material_id_count 15   

# works with single qsip_sample_data object

    Code
      get_object_summary(example_sample_object)
    Output
      # A tibble: 2 x 2
        metric                   value
        <chr>                    <chr>
      1 source_material_id_count 15   
      2 sample_id_count          284  

# works with single qsip_feature_data object

    Code
      get_object_summary(example_feature_object)
    Output
      # A tibble: 4 x 2
        metric           value 
        <chr>            <chr> 
      1 feature_id_count 2030  
      2 sample_id_count  284   
      3 data_type        counts
      4 taxonomy         FALSE 

# works with single qsip_data object

    Code
      get_object_summary(example_qsip_object)
    Output
      # A tibble: 6 x 2
        metric           none        
        <chr>            <chr>       
      1 feature_id_count 2030 of 2030
      2 sample_id_count  284         
      3 filtered         FALSE       
      4 resampled        FALSE       
      5 eaf              FALSE       
      6 growth           FALSE       

# works with single qsip_data object with a group

    Code
      get_object_summary(qsip_normal_strict_filtered)
    Output
      # A tibble: 6 x 2
        metric           none      
        <chr>            <chr>     
      1 feature_id_count 74 of 2030
      2 sample_id_count  284       
      3 filtered         TRUE      
      4 resampled        FALSE     
      5 eaf              FALSE     
      6 growth           FALSE     

# works with a named list of qsip_data objects

    Code
      get_object_summary(li)
    Output
      # A tibble: 6 x 3
        metric           none         none_1      
        <chr>            <chr>        <chr>       
      1 feature_id_count 2030 of 2030 2030 of 2030
      2 sample_id_count  284          284         
      3 filtered         FALSE        FALSE       
      4 resampled        FALSE        FALSE       
      5 eaf              FALSE        FALSE       
      6 growth           FALSE        FALSE       

