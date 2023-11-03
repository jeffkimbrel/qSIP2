# snapshots look as expected

    Code
      summarize_EAF_values(test_qsip)
    Message
      Confidence level = 0.9
    Output
      # A tibble: 74 x 5
         feature_id observed_EAF mean_resampled_EAF   lower  upper
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>
       1 ASV_1          -0.0153            -0.0147  -0.0521 0.0257
       2 ASV_10          0.113              0.113    0.0853 0.144 
       3 ASV_104         0.106              0.106    0.0699 0.144 
       4 ASV_108         0.210              0.211    0.0938 0.305 
       5 ASV_11          0.375              0.375    0.339  0.409 
       6 ASV_112         0.208              0.208    0.174  0.243 
       7 ASV_114         0.193              0.194    0.126  0.269 
       8 ASV_119        -0.00141           -0.00193 -0.0490 0.0415
       9 ASV_12          0.376              0.377    0.345  0.406 
      10 ASV_13          0.0670             0.0676   0.0453 0.0927
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
       1 ASV_1          -0.0153            -0.0147  -0.0588 0.0330
       2 ASV_10          0.113              0.113    0.0811 0.146 
       3 ASV_104         0.106              0.106    0.0646 0.154 
       4 ASV_108         0.210              0.211    0.0740 0.323 
       5 ASV_11          0.375              0.375    0.334  0.413 
       6 ASV_112         0.208              0.208    0.166  0.250 
       7 ASV_114         0.193              0.194    0.117  0.284 
       8 ASV_119        -0.00141           -0.00193 -0.0586 0.0480
       9 ASV_12          0.376              0.377    0.341  0.409 
      10 ASV_13          0.0670             0.0676   0.0399 0.0976
      # i 64 more rows

