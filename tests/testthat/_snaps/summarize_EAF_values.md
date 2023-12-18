# snapshots look as expected

    Code
      summarize_EAF_values(test_qsip)
    Message
      Confidence level = 0.9
    Output
      # A tibble: 74 x 5
         feature_id observed_EAF mean_resampled_EAF   lower  upper
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>
       1 ASV_1          -0.0153            -0.0150  -0.0513 0.0239
       2 ASV_10          0.113              0.113    0.0822 0.145 
       3 ASV_104         0.106              0.106    0.0663 0.143 
       4 ASV_108         0.210              0.209    0.0903 0.305 
       5 ASV_11          0.375              0.375    0.335  0.410 
       6 ASV_112         0.208              0.207    0.172  0.242 
       7 ASV_114         0.193              0.193    0.123  0.274 
       8 ASV_119        -0.00141           -0.00100 -0.0485 0.0437
       9 ASV_12          0.376              0.376    0.341  0.407 
      10 ASV_13          0.0670             0.0669   0.0427 0.0912
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
       1 ASV_1          -0.0153            -0.0150  -0.0560 0.0307
       2 ASV_10          0.113              0.113    0.0773 0.151 
       3 ASV_104         0.106              0.106    0.0593 0.150 
       4 ASV_108         0.210              0.209    0.0714 0.325 
       5 ASV_11          0.375              0.375    0.327  0.414 
       6 ASV_112         0.208              0.207    0.164  0.247 
       7 ASV_114         0.193              0.193    0.115  0.291 
       8 ASV_119        -0.00141           -0.00100 -0.0541 0.0512
       9 ASV_12          0.376              0.376    0.334  0.411 
      10 ASV_13          0.0670             0.0669   0.0361 0.0947
      # i 64 more rows

