# snapshots look as expected

    Code
      summarize_EAF_values(test_qsip)
    Message
      Confidence level = 0.9
    Output
      # A tibble: 74 x 5
         feature_id observed_EAF mean_resampled_EAF   lower  upper
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>
       1 ASV_1          -0.0153            -0.0155  -0.0528 0.0227
       2 ASV_10          0.113              0.112    0.0822 0.141 
       3 ASV_104         0.106              0.105    0.0677 0.144 
       4 ASV_108         0.210              0.209    0.0946 0.307 
       5 ASV_11          0.375              0.374    0.334  0.409 
       6 ASV_112         0.208              0.207    0.171  0.243 
       7 ASV_114         0.193              0.192    0.122  0.269 
       8 ASV_119        -0.00141           -0.00207 -0.0486 0.0425
       9 ASV_12          0.376              0.376    0.341  0.406 
      10 ASV_13          0.0670             0.0664   0.0402 0.0922
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
       1 ASV_1          -0.0153            -0.0155  -0.0607 0.0274
       2 ASV_10          0.113              0.112    0.0757 0.146 
       3 ASV_104         0.106              0.105    0.0589 0.153 
       4 ASV_108         0.210              0.209    0.0732 0.321 
       5 ASV_11          0.375              0.374    0.327  0.413 
       6 ASV_112         0.208              0.207    0.162  0.248 
       7 ASV_114         0.193              0.192    0.111  0.280 
       8 ASV_119        -0.00141           -0.00207 -0.0561 0.0485
       9 ASV_12          0.376              0.376    0.333  0.410 
      10 ASV_13          0.0670             0.0664   0.0359 0.0963
      # i 64 more rows

