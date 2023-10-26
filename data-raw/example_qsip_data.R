minimal <- readxl::read_excel("~/OD/Soils_SFA/analysis/qSIP_refactor/qsip2_example_data/qSIP_example.xlsx") |>
  dplyr::filter(sample_id %in% c("149", "150", "151", "152", "178", "179", "180", "161", "162", "163", "164", "200", "201", "202", "203")) |>
  dplyr::mutate(Fraction = stringr::str_remove(Fraction, "F")) |>
  dplyr::rename(source = sample_id, sample = SAMPLE) |>
  dplyr::mutate(source = paste("S", source, sep = "")) |>
  dplyr::select(-Root.Treatment)

example_source_df <- minimal |>
  dplyr::group_by(source) |>
  dplyr::mutate(total_copies_per_g = sum(avg_16S_g_soil)) |>
  dplyr::mutate(total_dna = sum(dna_conc)) |>
  dplyr::select(source, total_copies_per_g, total_dna, Isotope, Moisture) |>
  unique() |>
  dplyr::ungroup()

example_sample_df <- minimal |>
  dplyr::select(sample, source, Fraction, density_g_ml, dna_conc, avg_16S_g_soil) |>
  dplyr::mutate(dna_conc = ifelse(dna_conc < 0, 0, dna_conc))

asv_table <- readr::read_tsv("~/OD/Soils_SFA/analysis/qSIP_refactor/qsip2_example_data/table.txt")

example_feature_df <- asv_table |>
  dplyr::select(ASV, all_of(minimal$sample)) |>
  tidyr::pivot_longer(cols = c(everything(), -ASV)) |>
  dplyr::filter(value > 20) |>
  tidyr::pivot_wider(values_fill = 0) |>
  dplyr::mutate(ASV = stringr::str_replace(ASV, "DRIP16S", "ASV"))

# make qsip object
df <- example_source_df |>
  dplyr::mutate(isotopolog = "glucose")

example_source_object <- qsip_source_data(df,
  isotope = "Isotope",
  isotopolog = "isotopolog",
  source_mat_id = "source"
)


df <- example_sample_df |>
  add_gradient_pos_rel_amt(source_mat_id = "source", amt = "avg_16S_g_soil") |>
  dplyr::mutate(Fraction = as.integer(Fraction))

example_sample_object <- qsip_sample_data(df,
  sample_id = "sample",
  source_mat_id = "source",
  gradient_position = "Fraction",
  gradient_pos_density = "density_g_ml",
  gradient_pos_amt = "avg_16S_g_soil",
  gradient_pos_rel_amt = "gradient_pos_rel_amt"
)


example_feature_object <- qsip_feature_data(example_feature_df,
  feature_id = "ASV",
  type = "counts"
)


example_qsip_object <- qsip_data(
  example_source_object,
  example_sample_object,
  example_feature_object
)



# save
usethis::use_data(example_source_df, overwrite = TRUE)
usethis::use_data(example_sample_df, overwrite = TRUE)
usethis::use_data(example_feature_df, overwrite = TRUE)

usethis::use_data(example_source_object, overwrite = TRUE)
usethis::use_data(example_sample_object, overwrite = TRUE)
usethis::use_data(example_feature_object, overwrite = TRUE)

usethis::use_data(example_qsip_object, overwrite = TRUE)
