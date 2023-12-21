# resampling works correctly

    Code
      calculate_resampled_wads(1, test_wad, type = "labeled")
    Output
      # A tibble: 6 x 6
        feature_id type    resample labeled_1 labeled_2 labeled_3
        <chr>      <chr>      <dbl>     <dbl>     <dbl>     <dbl>
      1 1          labeled        1      1.70      1.70      1.70
      2 2          labeled        1      1.72      1.72      1.72
      3 3          labeled        1      1.74      1.74      1.74
      4 4          labeled        1      1.72      1.73      1.73
      5 5          labeled        1      1.71      1.72      1.72
      6 6          labeled        1      1.73      1.73      1.73

# dataframes with only one column still complete

    Code
      calculate_resampled_wads(1, data.frame(A = c(1, 2, 1.7)), type = "labeled")
    Output
      # A tibble: 3 x 4
        feature_id type    resample labeled_1
        <chr>      <chr>      <dbl>     <dbl>
      1 1          labeled        1       1  
      2 2          labeled        1       2  
      3 3          labeled        1       1.7

