# taxonomy related ##############
dplyr::select(example_feature_object@data, feature_id) |>
  dplyr::mutate(genus = "test", species = "test") |>
  saveRDS(file = "tests/testthat/fixtures/add_taxonomy_testdf.rds")

readRDS(file = "tests/testthat/fixtures/add_taxonomy_testdf.rds") |>
  rbind(tibble::tibble("feature_id" = "not_found", "genus" = "genus", "species" = "species")) |>
  saveRDS(file = "tests/testthat/fixtures/add_taxonomy_testdf_with_error.rds")

# wad matrix ##############
data.frame(
  labeled_1 = c(1.70130927995061, 1.71842281966778, 1.73540734122641, 1.72122157361111, 1.7121411962124, 1.73366339148623),
  labeled_2 = c(1.70489583061372, 1.72034693345524, 1.73618421172493, 1.7294567555353, 1.71751884929873, 1.73397109427623),
  labeled_3 = c(1.70130927995061, 1.71842281966778, 1.73540734122641, 1.72122157361111, 1.7121411962124, 1.73366339148623)
) |>
  saveRDS(file = "tests/testthat/fixtures/wad_matrix.rds")

# Z matrix ##############
data.frame(
  labeled = c(1.6, 1.65, 1.75),
  unlabeled = c(1.6, 1.675, 1.8)
) |>
  saveRDS(file = "tests/testthat/fixtures/Z_matrix.rds")

# qsip objects ##############
## normal strict
example_qsip_object |>
  run_feature_filter(
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S178", "S179", "S180"),
    min_unlabeled_sources = 3,
    min_labeled_sources = 3,
    min_unlabeled_fractions = 6,
    min_labeled_fractions = 6
  ) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_normal_strict_filtered.rds")

readRDS(file = "tests/testthat/fixtures/qsip_normal_strict_filtered.rds") |>
  run_resampling(resamples = 1000, with_seed = 43) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_normal_strict_resampled.rds")

readRDS(file = "tests/testthat/fixtures/qsip_normal_strict_resampled.rds") |>
  run_EAF_calculations() |>
  saveRDS(file = "tests/testthat/fixtures/qsip_normal_strict_EAF.rds")


readRDS(file = "tests/testthat/fixtures/qsip_normal_strict_filtered.rds") |>
  run_resampling(resamples = 1000, with_seed = 43, allow_failures = TRUE) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_normal_failures_resampled.rds")

readRDS(file = "tests/testthat/fixtures/qsip_normal_failures_resampled.rds") |>
  run_EAF_calculations() |>
  saveRDS(file = "tests/testthat/fixtures/qsip_normal_failures_EAF.rds")


## drought strict
example_qsip_object |>
  run_feature_filter(
    unlabeled_source_mat_ids = get_all_by_isotope(example_qsip_object, "12C"),
    labeled_source_mat_ids = c("S200", "S201", "S202", "S203"),
    min_unlabeled_sources = 4,
    min_labeled_sources = 4,
    min_unlabeled_fractions = 6,
    min_labeled_fractions = 6
  ) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_drought_strict_filtered.rds")

readRDS(file = "tests/testthat/fixtures/qsip_drought_strict_filtered.rds") |>
  run_resampling(resamples = 1000, with_seed = 43) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_drought_strict_resampled.rds")

readRDS(file = "tests/testthat/fixtures/qsip_drought_strict_resampled.rds") |>
  run_EAF_calculations() |>
  saveRDS(file = "tests/testthat/fixtures/qsip_drought_strict_EAF.rds")


multi_qsip = list("normal" = readRDS(file = "tests/testthat/fixtures/qsip_normal_strict_EAF.rds"),
                  "drought" = readRDS(file = "tests/testthat/fixtures/qsip_drought_strict_EAF.rds")) |>
  saveRDS(file = "tests/testthat/fixtures/multi_qsip_EAF.rds")





# Growth Data ##############

get_N_total_it(example_qsip_growth_object, t = 0) |>
  saveRDS(file = "tests/testthat/fixtures/N_total_i0.rds")

example_qsip_growth_object |>
  run_feature_filter(
    group = "Day 10",
    unlabeled_source_mat_ids = c("source_11", "source_14", "source_2", "source_5", "source_8"),
    labeled_source_mat_ids = c("source_12", "source_15", "source_3", "source_6", "source_9"),
    min_labeled_fractions = 4,
    min_unlabeled_fractions = 4
  ) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_growth_filtered.rds")

readRDS(file = "tests/testthat/fixtures/qsip_growth_filtered.rds") |>
  run_resampling(
    resamples = 100,
    with_seed = 1332,
    allow_failures = TRUE,
    quiet = TRUE
  ) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_growth_resampled.rds")

readRDS(file = "tests/testthat/fixtures/qsip_growth_resampled.rds") |>
  run_EAF_calculations(propO = 0.6) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_growth_EAF.rds")

readRDS(file = "tests/testthat/fixtures/qsip_growth_EAF.rds") |>
  run_growth_calculations(readRDS(file = "tests/testthat/fixtures/N_total_i0.rds")) |>
  saveRDS(file = "tests/testthat/fixtures/qsip_growth_rates.rds")

# Sample Data ##############
example_sample_df |>
  add_gradient_pos_rel_amt(source_mat_id = "source", amt = "avg_16S_g_soil") |>
  saveRDS(file = "tests/testthat/fixtures/sample_data_rel_amt.rds")


# Gradient position ##############
tibble::tibble("gradient_position" = c(1, 2, 3),
                    "gradient_pos_density" = c(1.6, 1.7, 1.8)) |>
  saveRDS(file = "tests/testthat/fixtures/gradient_pos_density_df.rds")

tibble::tibble("gradient_position" = c(1, 2, 3),
                         "gradient_pos_density" = c("1.6", "1.7", "1.8")) |>
  saveRDS(file = "tests/testthat/fixtures/gradient_pos_density_df_fail.rds")

tibble::tibble("gradient_position" = c(-1, 2, 3),
                         "gradient_pos_density" = c(1.6, 1.7, 1.8)) |>
  saveRDS(file = "tests/testthat/fixtures/gradient_pos_density_df_bulk.rds")

