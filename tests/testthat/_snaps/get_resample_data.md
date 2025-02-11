# no errors given

    Code
      get_resample_data(qsip_normal_strict_resampled)
    Output
      # A tibble: 74,000 x 13
         feature_id resample unlabeled_1 unlabeled_2 unlabeled_3 unlabeled_4
         <chr>         <int>       <dbl>       <dbl>       <dbl>       <dbl>
       1 ASV_1             1        1.70        1.70        1.70        1.70
       2 ASV_10            1        1.71        1.71        1.71        1.71
       3 ASV_104           1        1.71        1.71        1.71        1.71
       4 ASV_108           1        1.72        1.71        1.71        1.71
       5 ASV_11            1        1.71        1.71        1.71        1.71
       6 ASV_112           1        1.71        1.71        1.71        1.71
       7 ASV_114           1        1.71        1.71        1.71        1.71
       8 ASV_119           1        1.72        1.71        1.71        1.71
       9 ASV_12            1        1.71        1.71        1.71        1.71
      10 ASV_13            1        1.71        1.71        1.71        1.71
      # i 73,990 more rows
      # i 7 more variables: unlabeled_5 <dbl>, unlabeled_6 <dbl>, unlabeled_7 <dbl>,
      #   unlabeled_8 <dbl>, labeled_1 <dbl>, labeled_2 <dbl>, labeled_3 <dbl>

---

    Code
      get_resample_data(qsip_normal_strict_resampled, type = "labeled")
    Output
      # A tibble: 74,000 x 5
         feature_id resample labeled_1 labeled_2 labeled_3
         <chr>         <int>     <dbl>     <dbl>     <dbl>
       1 ASV_1             1      1.70      1.70      1.70
       2 ASV_10            1      1.72      1.72      1.72
       3 ASV_104           1      1.72      1.72      1.72
       4 ASV_108           1      1.73      1.73      1.73
       5 ASV_11            1      1.74      1.74      1.74
       6 ASV_112           1      1.72      1.72      1.72
       7 ASV_114           1      1.73      1.72      1.73
       8 ASV_119           1      1.72      1.71      1.72
       9 ASV_12            1      1.73      1.73      1.73
      10 ASV_13            1      1.71      1.72      1.71
      # i 73,990 more rows

---

    Code
      get_resample_data(qsip_normal_strict_resampled, type = "unlabeled")
    Output
      # A tibble: 74,000 x 10
         feature_id resample unlabeled_1 unlabeled_2 unlabeled_3 unlabeled_4
         <chr>         <int>       <dbl>       <dbl>       <dbl>       <dbl>
       1 ASV_1             1        1.70        1.70        1.70        1.70
       2 ASV_10            1        1.71        1.71        1.71        1.71
       3 ASV_104           1        1.71        1.71        1.71        1.71
       4 ASV_108           1        1.72        1.71        1.71        1.71
       5 ASV_11            1        1.71        1.71        1.71        1.71
       6 ASV_112           1        1.71        1.71        1.71        1.71
       7 ASV_114           1        1.71        1.71        1.71        1.71
       8 ASV_119           1        1.72        1.71        1.71        1.71
       9 ASV_12            1        1.71        1.71        1.71        1.71
      10 ASV_13            1        1.71        1.71        1.71        1.71
      # i 73,990 more rows
      # i 4 more variables: unlabeled_5 <dbl>, unlabeled_6 <dbl>, unlabeled_7 <dbl>,
      #   unlabeled_8 <dbl>

---

    Code
      get_resample_data(qsip_normal_strict_resampled, type = "labeled", pivot = TRUE)
    Output
      # A tibble: 222,000 x 5
         feature_id resample type    replicate   WAD
         <chr>         <int> <chr>   <chr>     <dbl>
       1 ASV_1             1 labeled 1          1.70
       2 ASV_1             1 labeled 2          1.70
       3 ASV_1             1 labeled 3          1.70
       4 ASV_10            1 labeled 1          1.72
       5 ASV_10            1 labeled 2          1.72
       6 ASV_10            1 labeled 3          1.72
       7 ASV_104           1 labeled 1          1.72
       8 ASV_104           1 labeled 2          1.72
       9 ASV_104           1 labeled 3          1.72
      10 ASV_108           1 labeled 1          1.73
      # i 221,990 more rows

