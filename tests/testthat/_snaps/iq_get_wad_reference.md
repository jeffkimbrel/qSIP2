# works as expected

    Code
      iq_get_wad_reference(example_qsip_object, quiet = F)
    Message
      212 reference features found with at least 5 fractions and 3 sources
    Output
      # A tibble: 212 x 7
         feature_id WAD_reference_mean      sd     cov WAD_reference_median n_sources
         <chr>                   <dbl>   <dbl>   <dbl>                <dbl>     <int>
       1 ASV_1                    1.70 0.00245 0.00144                 1.70         8
       2 ASV_10                   1.71 0.00183 0.00107                 1.71         8
       3 ASV_100                  1.71 0.00300 0.00175                 1.71         7
       4 ASV_101                  1.71 0.00570 0.00333                 1.71         6
       5 ASV_102                  1.72 0.00338 0.00197                 1.72         8
       6 ASV_103                  1.71 0.00315 0.00184                 1.71         8
       7 ASV_104                  1.71 0.00186 0.00108                 1.71         7
       8 ASV_105                  1.71 0.00275 0.00160                 1.71         6
       9 ASV_107                  1.72 0.00378 0.00220                 1.72         8
      10 ASV_108                  1.71 0.00257 0.00150                 1.71         4
      # i 202 more rows
      # i 1 more variable: mean_fractions <dbl>

---

    Code
      iq_get_wad_reference(example_qsip_object, quiet = T)
    Output
      # A tibble: 212 x 7
         feature_id WAD_reference_mean      sd     cov WAD_reference_median n_sources
         <chr>                   <dbl>   <dbl>   <dbl>                <dbl>     <int>
       1 ASV_1                    1.70 0.00245 0.00144                 1.70         8
       2 ASV_10                   1.71 0.00183 0.00107                 1.71         8
       3 ASV_100                  1.71 0.00300 0.00175                 1.71         7
       4 ASV_101                  1.71 0.00570 0.00333                 1.71         6
       5 ASV_102                  1.72 0.00338 0.00197                 1.72         8
       6 ASV_103                  1.71 0.00315 0.00184                 1.71         8
       7 ASV_104                  1.71 0.00186 0.00108                 1.71         7
       8 ASV_105                  1.71 0.00275 0.00160                 1.71         6
       9 ASV_107                  1.72 0.00378 0.00220                 1.72         8
      10 ASV_108                  1.71 0.00257 0.00150                 1.71         4
      # i 202 more rows
      # i 1 more variable: mean_fractions <dbl>

