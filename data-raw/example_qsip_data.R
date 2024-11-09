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
  dplyr::ungroup() |>
  dplyr::mutate(isotopolog = "glucose")

example_sample_df <- minimal |>
  dplyr::select(sample, source, Fraction, density_g_ml, dna_conc, avg_16S_g_soil) |>
  dplyr::mutate(dna_conc = ifelse(dna_conc < 0, 0, dna_conc)) |>
  dplyr::mutate(Fraction = as.integer(Fraction))

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


# growth object
q_feature <- readr::read_tsv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/qSIP2_TM/TM_uc_97_non_chimeras_otu_table_no_low_sample_filtered_.005_filtered_L6.txt") |>
  tidyr::pivot_longer(cols = c(dplyr::everything(), -Taxon)) |>
  dplyr::mutate(name = paste("sample", name, sep = "_")) |>
  tidyr::pivot_wider() |>
  dplyr::mutate(Taxon = paste("taxon", dplyr::row_number(), sep = "_")) |>
  qsip_feature_data(
    feature_id = "Taxon",
    type = "relative"
  )

q_samples <- readr::read_tsv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/qSIP2_TM/TM_fraction_data.txt") |>
  dplyr::filter(!is.na(qPCR.16S.copies.g.soil)) |> # removes the non-fractionated samples
  dplyr::mutate(fraction = ifelse(is.na(fraction), -1, fraction)) |> # convert bulk data to fraction -1
  dplyr::mutate(SampleID = paste("sample", SampleID, sep = "_")) |>
  dplyr::mutate(tube = paste("source", tube, sep = "_")) |>
  dplyr::select(tube, rep, tmt, fraction, SampleID, density.g.ml, qPCR.16S.copies.g.soil) |>
  qsip_sample_data(
    sample_id = "SampleID",
    source_mat_id = "tube",
    gradient_position = "fraction",
    gradient_pos_amt = "qPCR.16S.copies.g.soil",
    gradient_pos_density = "density.g.ml"
  )

q_source <- readr::read_tsv("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/qSIP2_TM/TM_replicate_data.txt") |>
  dplyr::mutate(tube = paste("source", tube, sep = "_")) |>
  dplyr::mutate(isotopolog = "water") |>
  dplyr::mutate(volume = 1) |>
  qsip_source_data(
    source_mat_id = "tube",
    isotopolog = "isotopolog",
    isotope = "tmt",
    time = "TIME",
    total_abundance = "qPCR.16S.copies.g.soil",
    volume = "volume"
  )

example_qsip_growth_object <- qsip_data(q_source, q_samples, q_feature)
example_qsip_growth_t0 <- get_N_total_it(example_qsip_growth_object, t = 0)



example_group_dataframe = readxl::read_excel("/Users/kimbrel1/Library/CloudStorage/Dropbox/working/qSIP/multiple_objects_test.xlsx")


# palettes
isotope_palette = c(
  "12C" = "#037bcf", "13C" = "#ff0000",
  "14N" = "#037bcf", "15N" = "#ff0000",
  "16O" = "#037bcf", "18O" = "#ff0000"
)

# JGI
jgi_mixes <- readxl::read_excel("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Soils_SFA/analysis/qSIP_refactor/JGI_mixes.xlsx") |>
  tidyr::pivot_longer(
    cols = tidyr::ends_with("stoichiometry"),
    names_to = "MIX",
    values_to = "STOICHIOMETRY"
  ) |>
  tidyr::drop_na(STOICHIOMETRY) |>
  dplyr::mutate(
    MIX = stringr::str_remove(MIX, "_stoichiometry"),
    MIX = stringr::str_remove(MIX, "MIX_")
  ) |>
  dplyr::mutate(RATIO = STOICHIOMETRY / sum(STOICHIOMETRY), .by = MIX) |>
  dplyr::rename(feature_id = ID) |>
  dplyr::arrange(MIX)

# save
usethis::use_data(example_source_df, overwrite = TRUE)
usethis::use_data(example_sample_df, overwrite = TRUE)
usethis::use_data(example_feature_df, overwrite = TRUE)

usethis::use_data(example_source_object, overwrite = TRUE)
usethis::use_data(example_sample_object, overwrite = TRUE)
usethis::use_data(example_feature_object, overwrite = TRUE)

usethis::use_data(example_qsip_object, overwrite = TRUE)

usethis::use_data(example_qsip_growth_object, overwrite = TRUE)
usethis::use_data(example_qsip_growth_t0, overwrite = TRUE)

usethis::use_data(example_group_dataframe, overwrite = TRUE)

usethis::use_data(isotope_palette, overwrite = TRUE)
usethis::use_data(jgi_mixes, overwrite = TRUE)
