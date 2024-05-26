#' Run comparison groups
#'

run_comparison_groups = function(qsip_data_object, groups) {



  # make sure all columns contain validate isotope names. "group" is skipped
  ## commenting this out for now as I am opting for "unlabeled" and "labeled" as the group names in the external file
  # if (!is.null(validate_isotopes(colnames(groups)[colnames(groups) != "group"]))) {
  #   stop("Invalid isotope names in groups")
  # }

  # make sure all source_mat_ids in the isotope columns are in the qsip_data_object
  source_mat_ids_in_groups = unlist(groups[, colnames(groups) != "group"]) |>
    enframe() |>
    separate_rows(value, sep = ",") |>
    mutate(value = str_trim(value)) |>
    pull(value)

  if (length(setdiff(source_mat_ids_in_groups, get_source_mat_ids(qsip_data_object))) > 0) {
    stop("Invalid source_mat_ids in group dataframe")
  }

  group_list = groups %>%
    pivot_longer(cols = -group, names_to = "isotope", values_to = "source_mat_ids") %>%
    split(.$group) %>%
    map(~.x %>%
          select(-group) |>
          mutate(source_mat_ids = str_remove_all(source_mat_ids, " ")) %>%
          mutate(source_mat_ids = str_split(source_mat_ids, ",")) |>
          deframe())

  # print the name of each element in group_list
  o = purrr::map2(group_list, names(group_list), \(i, j) test_func(i, j))
  o
}


test_func = function(l, n) {
  n
}


