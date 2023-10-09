minimal = readxl::read_excel("~/Desktop/qSIP_example.xlsx") |>
  filter(sample_id %in% c("149", "150", "151", "152", "178", "179", "180", "161", "162", "163", "164", "200", "201", "202", "203")) |>
  mutate(Fraction = stringr::str_remove(Fraction, "F")) |>
  rename(source = sample_id, sample = SAMPLE) |>
  mutate(source = paste("S", source, sep = "")) |>
  select(-Root.Treatment)

example_qsip_source_data = minimal |>
  dplyr::group_by(source) |>
  dplyr::mutate(total_copies_per_g = sum(avg_16S_g_soil)) |>
  dplyr::mutate(total_dna = sum(dna_conc)) |>
  dplyr::select(source, total_copies_per_g, total_dna, Isotope, Moisture) |>
  unique() |>
  dplyr::ungroup()

example_qsip_sample_data = minimal |>
  dplyr::select(sample, source, Fraction, density_g_ml, dna_conc, avg_16S_g_soil) |>
  mutate(dna_conc = ifelse(dna_conc < 0, 0, dna_conc))

asv_table = readr::read_tsv("~/Desktop/table.txt")

example_qsip_feature_data = asv_table |>
  select(ASV, all_of(minimal_sample$sample)) |>
  pivot_longer(cols = c(everything(), -ASV)) |>
  filter(value > 20) |>
  pivot_wider(values_fill = 0) |>
  mutate(ASV = str_replace(ASV, "DRIP16S", "ASV"))

usethis::use_data(example_qsip_source_data, overwrite = TRUE)
usethis::use_data(example_qsip_sample_data, overwrite = TRUE)
usethis::use_data(example_qsip_feature_data, overwrite = TRUE)
