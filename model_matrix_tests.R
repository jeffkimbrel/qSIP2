# testing model.matrix

j = example_source_df |>
  mutate(Moisture = case_when(
    total_copies_per_g > 53528072 ~ "New",
    TRUE ~ Moisture
  ))

M = "~Moisture:Isotope"

model.matrix(~Moisture:Isotope, j)
model.frame(M, j)

# run with a dataframe
show_comparison_groups(example_source_df,
                       group = "Moisture",
                       source_mat_id = "source",
                       isotope = "Isotope")

# run with a qsip source object
show_comparison_groups(example_source_object,
                       group = "Moisture")

# run with a qsip object
show_comparison_groups(example_qsip_object,
                       group = "Moisture")
