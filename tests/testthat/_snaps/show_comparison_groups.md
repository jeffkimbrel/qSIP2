# Works as expected

    Code
      show_comparison_groups(example_qsip_object, group = "Moisture")
    Output
      # A tibble: 2 x 3
        Moisture `12C`                  `13C`                 
        <chr>    <chr>                  <chr>                 
      1 Normal   S149, S150, S151, S152 S178, S179, S180      
      2 Drought  S161, S162, S163, S164 S200, S201, S202, S203

---

    Code
      show_comparison_groups(example_source_object, group = "Moisture")
    Output
      # A tibble: 2 x 3
        Moisture `12C`                  `13C`                 
        <chr>    <chr>                  <chr>                 
      1 Normal   S149, S150, S151, S152 S178, S179, S180      
      2 Drought  S161, S162, S163, S164 S200, S201, S202, S203

---

    Code
      show_comparison_groups(example_source_df, group = "Moisture", source_mat_id = "source",
        isotope = "Isotope")
    Output
      # A tibble: 2 x 3
        Moisture `12C`                  `13C`                 
        <chr>    <chr>                  <chr>                 
      1 Normal   S149, S150, S151, S152 S178, S179, S180      
      2 Drought  S161, S162, S163, S164 S200, S201, S202, S203

