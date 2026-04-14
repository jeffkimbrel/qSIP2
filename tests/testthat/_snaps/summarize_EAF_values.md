# snapshots look as expected

    Code
      summarize_EAF_values(normal_qsip)
    Message
      i Confidence level = 0.9
    Output
      # A tibble: 74 x 10
         feature_id observed_EAF mean_resampled_EAF   lower  upper  pval
         <chr>             <dbl>              <dbl>   <dbl>  <dbl> <dbl>
       1 ASV_1          -0.0153            -0.0150  -0.0513 0.0239 0.488
       2 ASV_10          0.113              0.113    0.0822 0.145  0    
       3 ASV_104         0.106              0.106    0.0663 0.143  0    
       4 ASV_108         0.210              0.209    0.0903 0.305  0.004
       5 ASV_11          0.375              0.375    0.335  0.410  0    
       6 ASV_112         0.208              0.207    0.172  0.242  0    
       7 ASV_114         0.193              0.193    0.123  0.274  0    
       8 ASV_119        -0.00141           -0.00100 -0.0485 0.0437 0.986
       9 ASV_12          0.376              0.376    0.341  0.407  0    
      10 ASV_13          0.0670             0.0669   0.0427 0.0912 0    
      # i 64 more rows
      # i 4 more variables: labeled_resamples <int>, unlabeled_resamples <int>,
      #   labeled_sources <int>, unlabeled_sources <int>

---

    Code
      summarize_EAF_values(normal_qsip, confidence = 0.95)
    Message
      i Confidence level = 0.95
    Output
      # A tibble: 74 x 10
         feature_id observed_EAF mean_resampled_EAF   lower  upper  pval
         <chr>             <dbl>              <dbl>   <dbl>  <dbl> <dbl>
       1 ASV_1          -0.0153            -0.0150  -0.0560 0.0307 0.488
       2 ASV_10          0.113              0.113    0.0773 0.151  0    
       3 ASV_104         0.106              0.106    0.0593 0.150  0    
       4 ASV_108         0.210              0.209    0.0714 0.325  0.004
       5 ASV_11          0.375              0.375    0.327  0.414  0    
       6 ASV_112         0.208              0.207    0.164  0.247  0    
       7 ASV_114         0.193              0.193    0.115  0.291  0    
       8 ASV_119        -0.00141           -0.00100 -0.0541 0.0512 0.986
       9 ASV_12          0.376              0.376    0.334  0.411  0    
      10 ASV_13          0.0670             0.0669   0.0361 0.0947 0    
      # i 64 more rows
      # i 4 more variables: labeled_resamples <int>, unlabeled_resamples <int>,
      #   labeled_sources <int>, unlabeled_sources <int>

# works on lists of qsip objects

    Code
      summarize_EAF_values(multi_qsip)
    Message
      i Confidence level = 0.9
    Output
      # A tibble: 155 x 11
         group  feature_id observed_EAF mean_resampled_EAF   lower  upper  pval
         <chr>  <chr>             <dbl>              <dbl>   <dbl>  <dbl> <dbl>
       1 normal ASV_1          -0.0153            -0.0150  -0.0513 0.0239 0.488
       2 normal ASV_10          0.113              0.113    0.0822 0.145  0    
       3 normal ASV_104         0.106              0.106    0.0663 0.143  0    
       4 normal ASV_108         0.210              0.209    0.0903 0.305  0.004
       5 normal ASV_11          0.375              0.375    0.335  0.410  0    
       6 normal ASV_112         0.208              0.207    0.172  0.242  0    
       7 normal ASV_114         0.193              0.193    0.123  0.274  0    
       8 normal ASV_119        -0.00141           -0.00100 -0.0485 0.0437 0.986
       9 normal ASV_12          0.376              0.376    0.341  0.407  0    
      10 normal ASV_13          0.0670             0.0669   0.0427 0.0912 0    
      # i 145 more rows
      # i 4 more variables: labeled_resamples <int>, unlabeled_resamples <int>,
      #   labeled_sources <int>, unlabeled_sources <int>

