# snapshots look as expected

    Code
      summarize_EAF_values(test_qsip)
    Message
      Confidence level = 0.9
    Output
      # A tibble: 74 x 5
         feature_id observed_EAF mean_resampled_EAF   lower  upper
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>
       1 ASV_1          -0.0153           -0.0145   -0.0524 0.0224
       2 ASV_10          0.113             0.113     0.0828 0.143 
       3 ASV_104         0.106             0.106     0.0684 0.145 
       4 ASV_108         0.210             0.208     0.0955 0.311 
       5 ASV_11          0.375             0.375     0.336  0.408 
       6 ASV_112         0.208             0.208     0.174  0.244 
       7 ASV_114         0.193             0.194     0.121  0.272 
       8 ASV_119        -0.00141          -0.000915 -0.0501 0.0431
       9 ASV_12          0.376             0.376     0.342  0.406 
      10 ASV_13          0.0670            0.0668    0.0427 0.0912
      # i 64 more rows

---

    Code
      summarize_EAF_values(test_qsip, confidence = 0.95)
    Message
      Confidence level = 0.95
    Output
      # A tibble: 74 x 5
         feature_id observed_EAF mean_resampled_EAF   lower  upper
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>
       1 ASV_1          -0.0153           -0.0145   -0.0586 0.0294
       2 ASV_10          0.113             0.113     0.0790 0.147 
       3 ASV_104         0.106             0.106     0.0648 0.154 
       4 ASV_108         0.210             0.208     0.0692 0.321 
       5 ASV_11          0.375             0.375     0.329  0.413 
       6 ASV_112         0.208             0.208     0.166  0.249 
       7 ASV_114         0.193             0.194     0.111  0.287 
       8 ASV_119        -0.00141          -0.000915 -0.0584 0.0485
       9 ASV_12          0.376             0.376     0.335  0.411 
      10 ASV_13          0.0670            0.0668    0.0392 0.0963
      # i 64 more rows

