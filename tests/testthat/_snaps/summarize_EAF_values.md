# snapshots look as expected

    Code
      summarize_EAF_values(test_qsip)
    Message
      Confidence level = 0.9
    Output
      # A tibble: 74 x 9
         feature_id observed_EAF mean_resampled_EAF   lower  upper labeled_resamples
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>             <int>
       1 ASV_1          -0.0153            -0.0150  -0.0513 0.0239              1000
       2 ASV_10          0.113              0.113    0.0822 0.145               1000
       3 ASV_104         0.106              0.106    0.0663 0.143               1000
       4 ASV_108         0.210              0.209    0.0903 0.305               1000
       5 ASV_11          0.375              0.375    0.335  0.410               1000
       6 ASV_112         0.208              0.207    0.172  0.242               1000
       7 ASV_114         0.193              0.193    0.123  0.274               1000
       8 ASV_119        -0.00141           -0.00100 -0.0485 0.0437              1000
       9 ASV_12          0.376              0.376    0.341  0.407               1000
      10 ASV_13          0.0670             0.0669   0.0427 0.0912              1000
      # i 64 more rows
      # i 3 more variables: unlabeled_resamples <int>, labeled_sources <int>,
      #   unlabeled_sources <int>

---

    Code
      summarize_EAF_values(test_qsip, confidence = 0.95)
    Message
      Confidence level = 0.95
    Output
      # A tibble: 74 x 9
         feature_id observed_EAF mean_resampled_EAF   lower  upper labeled_resamples
         <chr>             <dbl>              <dbl>   <dbl>  <dbl>             <int>
       1 ASV_1          -0.0153            -0.0150  -0.0560 0.0307              1000
       2 ASV_10          0.113              0.113    0.0773 0.151               1000
       3 ASV_104         0.106              0.106    0.0593 0.150               1000
       4 ASV_108         0.210              0.209    0.0714 0.325               1000
       5 ASV_11          0.375              0.375    0.327  0.414               1000
       6 ASV_112         0.208              0.207    0.164  0.247               1000
       7 ASV_114         0.193              0.193    0.115  0.291               1000
       8 ASV_119        -0.00141           -0.00100 -0.0541 0.0512              1000
       9 ASV_12          0.376              0.376    0.334  0.411               1000
      10 ASV_13          0.0670             0.0669   0.0361 0.0947              1000
      # i 64 more rows
      # i 3 more variables: unlabeled_resamples <int>, labeled_sources <int>,
      #   unlabeled_sources <int>

# works on lists of qsip objects

    Code
      summarize_EAF_values(list(A = test_qsip))
    Message
      Confidence level = 0.9
    Output
      # A tibble: 74 x 10
         group feature_id observed_EAF mean_resampled_EAF   lower  upper
         <chr> <chr>             <dbl>              <dbl>   <dbl>  <dbl>
       1 A     ASV_1          -0.0153            -0.0150  -0.0513 0.0239
       2 A     ASV_10          0.113              0.113    0.0822 0.145 
       3 A     ASV_104         0.106              0.106    0.0663 0.143 
       4 A     ASV_108         0.210              0.209    0.0903 0.305 
       5 A     ASV_11          0.375              0.375    0.335  0.410 
       6 A     ASV_112         0.208              0.207    0.172  0.242 
       7 A     ASV_114         0.193              0.193    0.123  0.274 
       8 A     ASV_119        -0.00141           -0.00100 -0.0485 0.0437
       9 A     ASV_12          0.376              0.376    0.341  0.407 
      10 A     ASV_13          0.0670             0.0669   0.0427 0.0912
      # i 64 more rows
      # i 4 more variables: labeled_resamples <int>, unlabeled_resamples <int>,
      #   labeled_sources <int>, unlabeled_sources <int>

