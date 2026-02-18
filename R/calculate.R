#' Get the number of relevant atoms per nucleotide (internal)
#'
#' Carbon and nitrogen atoms varies with GC content, but oxygen content is
#' constant for DNA regardless of GC content.
#'
#' @param G (*numeric*) GC percentage
#' @param isotope (*string*) The isotope to use for calculations... either 13C, 15N or 18O
#'
#' @returns (*numeric*) The number of atoms per nucleotide
#'
#' @keywords internal


calculate_atoms <- function(G, isotope) {

  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  if (!is.numeric(G)) {
    stop(glue::glue("G should be class <numeric>, not {class(G)[1]}"), call. = FALSE)
  }

  if (isotope == "13C") {
    C_atoms <- (-0.5 * G) + 10
    return(C_atoms)
  } else if (isotope == "15N") {
    N_atoms <- (0.5 * G) + 3.5
    return(N_atoms)
  } else if (isotope == "18O") {
    O_atoms <- 6
    return(O_atoms)
  }
}


#' Calculate EAF value (internal)
#'
#' @param M_labeled Molecular weight of the labeled feature
#' @param M Molecular weight of the unlabeled feature
#' @param M_labeledmax Theoretical molecular weight of a completely labeled feature
#' @param isotope The isotope to determine which calculation to run
#'
#' @keywords internal
#'
#' @returns EAF value

calculate_EAF <- function(M_labeled, M, M_labeledmax, isotope) {

  # make sure Ms are numerics
  if (!is.numeric(M)) {
    stop(glue::glue("M should be class <numeric>, not {class(M)[1]}"), call. = FALSE)
  } else if (!is.numeric(M_labeled)) {
    stop(glue::glue("M_labeled should be class <numeric>, not {class(M_labeled)[1]}"), call. = FALSE)
  } else if (!is.numeric(M_labeledmax)) {
    stop(glue::glue("M_labeledmax should be class <numeric>, not {class(M_labeledmax)[1]}"), call. = FALSE)
  }


  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  EAF <- (M_labeled - M) / (M_labeledmax - M) * completely_labeled_values(isotope)

  return(EAF)
}






#' Internal function for resampling WAD values (internal)
#'
#' Designed as a purrr::map() function called by run_resampling.R
#'
#' @param i (*integer*) The specific iteration of the resampling
#' @param wad_data (*dataframe*) A WAD dataframe to resample columns from
#' @param type (*string*) Text for whether the wad data is from labeled or unlabeled data
#' @param allow_failures (*logical*) Whether to allow failures in the resampling
#'
#' @returns The resampling data that will be boot in `@resamples`
#'
#' @keywords internal
#' @export


calculate_resampled_wads <- function(i, wad_data, type, allow_failures = FALSE) {

  # make sure all data is numeric or NA
  stopifnot("wad dataframe to resample from contains non-numeric data" = all(sapply(wad_data, is.numeric)))

  # bind variables
  feature_id <- NULL

  # make a new names vector to abstract away the real names into a numbered list of names
  new_names <- c("feature_id", paste(type, seq(1:(ncol(wad_data))), sep = "_"))
  wad_data_resampled <- wad_data[, sample(ncol(wad_data), replace = T, size = ncol(wad_data)), drop = FALSE]

  if (allow_failures == FALSE) {
    # double check the dimensions remain the same after removing all rows with NA.
    wad_data_resampled_noNA = wad_data_resampled[rowSums(is.na(wad_data_resampled)) != ncol(wad_data_resampled), ]
    if (methods::is(wad_data_resampled_noNA, "numeric")) {
      wad_data_resampled_noNA_length = length(wad_data_resampled_noNA)
    } else if (methods::is(wad_data_resampled_noNA, "data.frame")) {
      wad_data_resampled_noNA_length = nrow(wad_data_resampled_noNA)
    }

    if (identical(nrow(wad_data), wad_data_resampled_noNA_length) == FALSE) {
      stop(("Something went wrong with resampling...\nIt is possible that some resampled features contained only <NA> WAD values leading to a failure in calculate_Z().\nTry increasing your filtering stringency to remove features not found in most sources"), call. = FALSE)
    }
  } else if (allow_failures == TRUE) {
    # just remove rows that are all NAs... this will reduce the number of successful resamples reported for this feature/type


    wad_data_resampled = wad_data_resampled[rowSums(is.na(wad_data_resampled)) != ncol(wad_data_resampled), , drop = FALSE]

  }

  # save the original names, in case they are needed later
  wad_data_resampled_names <- colnames(wad_data_resampled)

  # bring the feature id from the rownames back to a column, repair the names,
  # add the resample # and the type, and arrange just for fun
  wad_data_resampled <- wad_data_resampled |>
    tibble::rownames_to_column("feature_id") |>
    tibble::as_tibble(.name_repair = ~new_names) |>
    dplyr::mutate(resample = i, .after = feature_id) |>
    dplyr::mutate(type = type, .after = "feature_id") |>
    dplyr::arrange(feature_id)

  wad_data_resampled
}






#' Calculate tube relative abundance (internal)
#'
#' The "tube level relative abundance" has two layers of relative abundance. It first
#' takes the `raw_abundance` counts per sample, and divides by the total to get
#' the `rel_abundance` values. Next, it takes these values and divides them by the
#' `gradient_pos_rel_amt` values stored at the source level which normalizes the `rel_abundance`
#' values by the amount of sample per fraction. This is the final value stored in
#' the `tube_rel_abundance` column.
#'
#' To speed up the calculations, this function removes `feature_ids` in a sample
#' if the abundance is zero.
#'
#' v0.10.3 updated this function to have different behavior for feature type 'normalized'.
#' This type should use the raw data in the feature table, putting those values into the
#' `tube_rel_abundance` column.
#'
#' @param source_data (*qsip_source_data*) A qSIP source data object
#' @param sample_data (*qsip_sample_data*) A qSIP sample data object
#' @param feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @returns A long format dataframe with one row per `feature_id` per `sample_id`
#'
#' @keywords internal


calculate_tube_rel_abundance <- function(source_data, sample_data, feature_data) {

  # make sure the objects are of the right type
  stopifnot("source_data should be of class <qsip_source_data>" = inherits(source_data, qsip_source_data))
  stopifnot("sample_data should be of class <qsip_sample_data>" = inherits(sample_data, qsip_sample_data))
  stopifnot("feature_data should be of class <qsip_feature_data>" = inherits(feature_data, qsip_feature_data))

  # bind variables
  feature_id <- raw_abundance <- sample_id <- source_mat_id <- rel_abundance <- gradient_pos_density <- gradient_pos_rel_amt <- tube_rel_abundance <- isotope <- tube_rel_abundance <- NULL

  # extract dataframes
  feature_df <- S7::prop(feature_data, "data")
  sample_df <- S7::prop(sample_data, "data") |>
    dplyr::select(-dplyr::any_of("isotope")) # remove isotope column if it exists (addresses issue #5)
  source_df <- S7::prop(source_data, "data")

  # make sure feature data type has compatible value
  stopifnot("feature <type> is unknown" = S7::prop(feature_data, "type") %in% c("counts", "coverage", "normalized", "relative"))

  if (S7::prop(feature_data, "type") %in% c("counts", "coverage", "relative")) {
    feature_df |> # start with raw feature data
      tidyr::pivot_longer(
        cols = c(dplyr::everything(), -feature_id), # pivot longer
        names_to = "sample_id",
        values_to = "raw_abundance"
      ) |>
      dplyr::filter(raw_abundance > 0) |> # remove features with no abundance
      dplyr::group_by(sample_id) |> # group to calculate per-sample relative abundance
      dplyr::mutate(rel_abundance = raw_abundance / sum(raw_abundance)) |> # do the calculation
      dplyr::ungroup() |>
      dplyr::left_join(sample_df, by = "sample_id") |> # add sample data to get the source_mat_id
      dplyr::filter(!is.na(source_mat_id)) |> # remove features that do not have a source id (this removes features found in the feature table but not the metadata)
      dplyr::left_join(source_df, by = "source_mat_id") |> # combine
      dplyr::select(feature_id, sample_id, rel_abundance, source_mat_id, gradient_pos_density, gradient_pos_rel_amt, isotope) |>
      dplyr::group_by(feature_id, source_mat_id, isotope) |>
      dplyr::mutate(tube_rel_abundance = rel_abundance * gradient_pos_rel_amt) |> # takes the sample-adjusted abundances and gets the source-adjusted abundances
      dplyr::ungroup() |>
      dplyr::select(feature_id, sample_id, source_mat_id, tube_rel_abundance, gradient_pos_density, gradient_pos_rel_amt)
  } else if (S7::prop(feature_data, "type") == "normalized") {
    message("normalized")
    feature_df |> # start with raw feature data
      tidyr::pivot_longer(
        cols = c(dplyr::everything(), -feature_id), # pivot longer
        names_to = "sample_id",
        values_to = "raw_abundance"
      ) |>
      dplyr::filter(raw_abundance > 0) |> # remove features with no abundance
      dplyr::left_join(sample_df, by = "sample_id") |> # add sample data to get the source_mat_id
      dplyr::filter(!is.na(source_mat_id)) |> # remove features that do not have a source id (this removes features found in the feature table but not the metadata)
      dplyr::left_join(source_df, by = "source_mat_id") |>
      dplyr::group_by(sample_id) |>
      dplyr::mutate(gradient_pos_rel_amt = sum(raw_abundance)) |>
      dplyr::ungroup() |>
      dplyr::select(feature_id, sample_id, source_mat_id, tube_rel_abundance = raw_abundance, gradient_pos_density, gradient_pos_rel_amt)
  }
}





#' Calculate weighted average density (WAD) values (internal)
#'
#' @param tube_rel_abundance (*dataframe*) Output from `calculate_tube_rel_abundance()`
#'
#' @returns A list with two objects, 1) a dataframe with WAD info for the feature_ids
#' found in at least one sample, and 2) the fraction counts for all feature_ids,
#' including those not found at all in some samples. It also prints a message to
#' the screen with a count of feature_ids that are entirely missing from at least
#' one sample.
#'
#' @keywords internal


calculate_wads <- function(tube_rel_abundance) {

  stopifnot("data is not class data.frame" = "data.frame" %in% class(tube_rel_abundance))

  stopifnot("feature_id column missing" = "feature_id" %in% colnames(tube_rel_abundance))
  stopifnot("source_mat_id column missing" = "source_mat_id" %in% colnames(tube_rel_abundance))
  stopifnot("tube_rel_abundance column missing" ="tube_rel_abundance" %in% colnames(tube_rel_abundance))
  stopifnot("gradient_pos_density column missing" ="gradient_pos_density" %in% colnames(tube_rel_abundance))

  # bind variables
  feature_id <- source_mat_id <- gradient_pos_density <- n_fractions <- WAD <- NULL

  wads <- tube_rel_abundance |>
    dplyr::group_by(feature_id, source_mat_id) |>
    dplyr::summarize(
      WAD = weighted.mean(gradient_pos_density, tube_rel_abundance),
      n_fractions = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(n_fractions)

  fraction_counts <- wads |>
    dplyr::select(-WAD) |>
    tidyr::pivot_wider(
      names_from = source_mat_id,
      values_from = n_fractions
    ) |>
    tidyr::pivot_longer(
      cols = c(dplyr::everything(), -feature_id),
      names_to = "source_mat_id",
      values_to = "n_fractions"
    ) |>
    dplyr::arrange(feature_id)

  # missing_feature_ids <- fraction_counts |>
  #   dplyr::filter(is.na(n_fractions)) |>
  #   dplyr::count(feature_id, name = feature_count) |>
  #   dplyr::arrange(feature_count) |>
  #   dplyr::count(feature_count, name = "counts") |>
  #   dplyr::summarize(S = sum(counts)) |>
  #   dplyr::pull(S)

  #message(glue::glue_col("WARNING: {red {missing_feature_ids}} feature_ids have no counts in one or more source_mat_ids"))

  return(list(
    "wads" = wads,
    "fraction_counts" = fraction_counts
  ))
}



#' Returns the max labeling of a given isotope (internal)
#'
#' @param isotope The heavy isotope (13C, 15N or 18O)
#'
#' @keywords internal
#'
#' @returns A number representing the max labeling of a given isotope

completely_labeled_values <- function(isotope) {

  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  if (isotope == "13C") {
    return(1 - (11237.2 / (1000000 + 11237.2)))
  } else if (isotope == "15N") {
    return(1 - ((1000000 / 272) / (1000000 + (1000000 / 272))))
  } else if (isotope == "18O") {
    return(1 - (2005.20 / (1000000 + 379.9 + 2005.20)))
  }
}





#' Calculate molecular weight of the labeled feature (internal)
#'
#' The GC value given to this equation is usually calculated from the density
#' value, not derived from the sequence itself.
#'
#' This function corresponds to equation 6 from Hungate, 2015.
#'
#' @param G (*numeric*) GC content of a feature, ranges from 0-1.
#'
#' @returns `M` is the molecular weight of a sequence with `G` GC content
#'
#' @keywords internal

calculate_M <- function(G) {

  if (!is.numeric(G)) {
    stop(glue::glue("G should be class <numeric>, not {class(G)}"), call. = FALSE)
  }

  M <- (G * 0.496) + 307.691
  M
}



#' Calculate molecular weight of the labeled feature (internal)
#'
#' This function corresponds to equation 7 from Hungate, 2015
#'
#' @param M (*numeric*) Molecular weight of the unlabeled feature
#' @param atom_count (*numeric*) The count of the relevant atoms (C, N or O)
#' @param isotope (*string*) The heavy isotope determining which calculation to run. Needs to be 13C, 15N or 18O
#'
#' @returns `M_labeledmax` is the theoretical maximum molecular weight the labeled feature could be
#'
#' @keywords internal

calculate_M_labeledmax <- function(M,
                                   atom_count,
                                   isotope) {

  validate_isotopes(isotope, isotope_list = c("13C", "15N", "18O"))

  if (!is.numeric(M)) {
    stop(glue::glue("M should be <numeric>, not {class(M)}"), call. = FALSE)
  }

  if (isotope == "13C") {
    # assumes unlabeled DNA already contains a minute amount of 13C (at the
    # natural abundance level of VPDB)
    M_labeledmax <- M + (atom_count * (1.008665 * (1000000 / (1000000 + 11237.2))))
    return(M_labeledmax)

  } else if (isotope == "15N") {
    # assumes unlabeled DNA already contains minute amounts of 15N (at the
    # natural abundance level of AIR-N2)
    M_labeledmax <- M + (atom_count * (1.008665 * (1000000 / (1000000 + (1000000 / 272)))))
    return(M_labeledmax)

  } else if (isotope == "18O") {
    # assumes unlabeled DNA already contains minute amounts of 18O and 17O
    # (at the natural abundance levels of those isotopes in VSMOW)
    # propO term removed v0.20.6
    M_labeledmax <- M + (atom_count * ((1.008665 * 2 * (1000000 / (1000000 + 379.9 + 2005.20))) + (1.008665 * 1 * (379.9 / (1000000 + 379.9 + 2005.20)))))
    return(M_labeledmax)
  }
}



#' Calculate molecular weight of the labeled feature (internal)
#'
#' @param M (*numeric*) Molecular weight of the unlabeled feature
#' @param W_lab_mean (*numeric*) WAD (or mean WAD) value of the labeled feature
#' @param W_unlab_mean (*numeric*) WAD (or mean WAD) value of the unlabeled feature
#'
#' @returns `M_labeled` is the molecular weight of the labeled feature
#'
#' @keywords internal

calculate_M_labeled <- function(M, W_lab_mean, W_unlab_mean) {

  if (any(!is.numeric(c(M, W_lab_mean, W_unlab_mean)))) {
    stop("some input values not class <numeric>")
  }

  M_labeled <- M * (W_lab_mean / W_unlab_mean)
  return(M_labeled)
}






#' Calculate GC% from density/WAD
#'
#' This function takes a `density` value and an optional `method` and returns the
#' predicted GC% content of a DNA sequence with that density.
#'
#' The `method` parameter changes the formula from that provided by McHugh &
#' Morrissey (`MM`, unpublished) or Schildkraut (`S`, 1962).
#'
#' This function corresponds to equation 5 from Hungate, 2015
#'
#' @param density (*numeric*) Density or WAD values
#' @param method (*string, default: MM*) The GC% calculation method
#'
#' @returns A vector of GC% values
#'
#' @export

calculate_gc_from_density <- function(density,
                                      method = "MM") {

  stopifnot("ERROR: density argument should be numeric" = is.numeric(density))

  if (method == "MM") {
    G <- (1 / 0.0835059954345993) * (density - 1.64605745338531)
    return(G)
  } else if (method == "S") {
    G <- (1 / 0.098) * (density - 1.66)
    return(G)
  } else {
    stop(glue::glue("ERROR: {method} is not a valid method for GC% calculation. Options are MM (default) or S."))
  }
}



#' Calculate GC% from a sequence
#'
#' This function takes a sequence and counts the number of G and C bases, and divides
#' by the total sequence length.
#'
#' @param sequence (*string*) Density or WAD values
#'
#' @returns A vector of GC% values
#'
#' @export


calculate_gc_from_sequence <- function(sequence) {
  if (is.character(sequence)) {

    # error if string contains characters other than A, C, G or T
    if (any(grepl("[^ACGTacgt]", sequence) == TRUE)) {
      stop("<sequence> appears to contain invalid characters (not ACGT/acgt)")
    }

    (stringr::str_count(sequence, "g|G") + stringr::str_count(sequence, "c|C")) / stringr::str_length(sequence)
  } else {
    stop("Please provide a string sequence to calculate GC%")
  }
}




#' Calculate the WAD difference Z (internal)
#'
#' This is just basic subtraction, so probably overkill to make a function!
#'
#' This function corresponds to equation 4 from Hungate, 2015
#'
#' @param labeled (*string*) The column with the labeled WAD or mean labeled WAD value
#' @param unlabeled (*string*) The column with the unlabeled WAD or mean unlabeled WAD value
#'
#' @returns a value for the difference between labeled and unlabeled
#'
#' @keywords internal

calculate_Z <- function(labeled, unlabeled) {
  if (any(is.na(c(
    labeled,
    unlabeled
  )))) {
    stop("Can't calculate Z - some WAD values are <NA>")
  }

  stopifnot("Can't calculate Z - some WAD values are not numeric" = is.numeric(c(labeled, unlabeled)))

  Z <- labeled - unlabeled
  return(Z)
}




#' Return the size of qsip data objects or each slot
#'
#' @param qsip_data_object A qsip object or list of objects
#' @param units Units to format the size in. Default is "auto"
#'
#' @export

qsip_object_size = function(qsip_data_object,
                            units = "auto") {

  if (isTRUE(is_qsip_data(qsip_data_object))) {
    l = S7::props(qsip_data_object)
    colname = "@slot"
  } else if (isTRUE(is_qsip_data_list(qsip_data_object))) {
    l = qsip_data_object
    colname = "$group"
  } else {
    stop("<qsip_data_object> must be a qsip object or list of qsip objects", call. = F)
  }

  purrr::map_df(l, function(x) {
    tibble::tibble(
      size_raw = object.size(x),
      size = size_raw %>% format(units = units)
    )
  }) |>
    #cbind("@slot" = names(j)) |>
    cbind(names = names(l)) |>
    dplyr::arrange(-size_raw) |>
    dplyr::select(!!colname := names, size)
}



#' Calculate M_heavy
#'
#' This is equation 4 from Koch, 2018
#'
#' @param propO (*numeric*) The proportion of oxygen coming from 18H2O versus other sources
#' @param M (*numeric*) The mass of the molecule
#'
#' @keywords internal

calculate_M_heavy <- function(propO, M) {

  if (propO > 1 | propO < 0) {
    stop("prop0 should be between 0 and 1", call. = FALSE)
  }

  if (!is.numeric(M)) {
    stop("M should be class <numeric>", call. = FALSE)
  }

  M_heavy <- (12.07747 * propO) + M

  return(M_heavy)
}



#' Calculate N_Light_it
#'
#' This is equation 3 from Koch, 2018
#'
#' @param N_total_it The copy number of feature i at timepoint t
#' @param M_heavy The theoretical molecular weight of 100% labeled sequence
#' @param M_labeled The molecular weight of the labeled sequence
#' @param M The molecular weight of the unlabeled sequence
#'
#' @keywords internal

calculate_N_light_it <- function(N_total_it, M_heavy, M_labeled, M) {

  N_Light_it <- N_total_it * ((M_heavy - M_labeled) / (M_heavy - M))

  return(N_Light_it)
}




#' Calculate death rate
#'
#' Equation 6 from Koch, 2018
#'
#' @param N_light_it The unlabeled copy number of feature i at timepoint t
#' @param N_total_i0 The copy number of feature i at timepoint 0
#' @param timepoint The timepoint at which the copy number is being measured
#' @param timepoint1 The timepoint that is being compared against
#' @param growth_model (*character, default: exponential*) The growth model to use. Must be either "exponential" or "linear"
#'
#' @keywords internal


calculate_di <- function(N_light_it,
                         N_total_i0,
                         timepoint,
                         timepoint1,
                         growth_model = "exponential") {
  if (!growth_model %in% c("exponential", "linear")) {
    stop(glue::glue("growth_model must be either 'exponential' or 'linear', not {growth_model}"), call. = FALSE)
  }

  if (growth_model == "exponential") {
    di <- log(N_light_it / N_total_i0) * (1 / (timepoint - timepoint1))
  } else if (growth_model == "linear") {
    di <- (N_light_it - N_total_i0) / (timepoint - timepoint1)
  }

  return(di)
}





#' Calculate birth rate
#'
#' Equation 7 from Koch, 2018
#'
#' @param N_total_it The total copy number of feature i at timepoint t
#' @param N_light_it The unlabeled copy number of feature i at timepoint t
#' @param timepoint The timepoint at which the copy number is being measured
#' @param timepoint1 The timepoint that is being compared against
#' @param growth_model (*character, default: exponential*) The growth model to use. Must be either "exponential" or "linear"
#'
#' @keywords internal

calculate_bi <- function(N_total_it,
                         N_light_it,
                         timepoint,
                         timepoint1,
                         growth_model = "exponential") {
  if (!growth_model %in% c("exponential", "linear")) {
    stop(glue::glue("growth_model must be either 'exponential' or 'linear', not {growth_model}"), call. = FALSE)
  }

  if (growth_model == "exponential") {
    bi <- log(N_total_it / N_light_it) * (1 / (timepoint - timepoint1))
  } else if (growth_model == "linear") {
    bi <- (N_total_it - N_light_it) / (timepoint - timepoint1)
  }

  return(bi)
}
