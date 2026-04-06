#' Get delta EAF values between contrast groupings
#'
#' Given a list of qSIP2 objects, this function will return the delta EAF value
#' for each feature_id, including the confidence interval, standard deviation,
#' and two p-values from 1) the bootstrap percentile test (`bs_pval`) and 2) the
#' Wald's z-test (`pval`).
#'
#' By default the function will do all pairwise comparisons of the groups in the
#' object list with the first listed as the "control" and the second as the
#' "treatment". Optionally, data frame with "control", "treatment" and
#' "contrast" columns can be provided to specify only the contrasts of interest
#' and to define the proper "control" vs. "treatment" groups. To ensure this
#' data frame is properly formatted (e.g. has the proper column names and all
#' groups are found in the qSIP2 object list), it must be validated through the
#' `validate_delta_EAF_contrasts()` function.
#'
#' Under the hood, for each feature_id this function subtracts the observed EAF
#' in the "treatment" group from the "control" and saves this as the \eqn{\Delta}
#' value. Then, it pulls the data from `run_resampling()` and for each resampling
#' \eqn{i} calculates the delta (so \eqn{\Delta_i = treatment_i - control_i}) saving
#' these values as a vector of delta distributions. The `bs_pval` is calculated
#' from this distribution, and `pval` is calculated from \eqn{\Delta} and the
#' standard deviation of the distribution.
#'
#' @param q a qSIP2 list object
#' @param contrasts A validated contrasts table
#' @param confidence (numeric, default: 0.95) confidence interval
#' @param quiet (*boolean*) If `TRUE`, suppresses messages
#'
#' @returns a tibble
#' @export

run_delta_EAF_contrasts = function(q, contrasts = NULL, confidence = 0.95, quiet = FALSE) { # keeping in the same order as validate_delta_EAF_contrasts

  is_qsip_data_list(q, error = TRUE)

  if (is.null(contrasts)) {
    cli::cli_alert_info("{.arg contrasts} not given so running all-by-all")
    contrasts = make_delta_EAF_contrasts(q)

  } else {
    contrasts = validate_delta_EAF_contrasts(q = q, contrasts = contrasts)
  }

  if (!is.numeric(confidence)) {
    cli::cli_abort("{.arg confidence} must be numeric, not {.cls {class(confidence)}}.")
  }
  if (confidence <= 0 | confidence >= 1) {
    cli::cli_abort("{.arg confidence} must be between 0 and 1, not {.val {confidence}}.")
  }

  if (isFALSE(quiet)) {
    cli::cli_alert_info("Confidence level = {confidence}")
  }

  q_nested = lapply(q, get_EAF_data) |>
    dplyr::bind_rows(.id = "group") |>
    dplyr::select(group, feature_id, resample, EAF) |>
    tidyr::nest(resampling = -feature_id)

  results = q_nested |>
    dplyr::mutate(delta_results = purrr::map(resampling, ~get_delta_distributions(.x, contrasts), .progress = "step 1/2: calculating deltas..."),
                  delta_distributions = purrr::map(delta_results, "delta_distributions"),
                  contrast_message = purrr::map_chr(delta_results, "status")) |>
    dplyr::mutate(delta = purrr::map2(delta_distributions, confidence, ~get_delta_stats(.x, .y), .progress = "step 2/2: summarizing delta statistics")) |>
    dplyr::select(feature_id, delta, contrast_message) |>
    tidyr::unnest(delta)

  # return message summary
  with_contrast_messages = results |>
    dplyr::filter(!is.na(contrast_message))

  with_bs_pval_message = results |>
    dplyr::filter(!is.na(bs_pval_message))

  if (isFALSE(quiet)) {
    cli::cli_alert_warning("there were {nrow(with_contrast_messages)} contrast and {nrow(with_bs_pval_message)} bs_pval result messages")
  }

  return(results)


}


#' Make an all-by-all contrasts table for delta EAF calculations
#'
#' This is an internal function that runs if a user does not provide a contrasts
#' table to `run_delta_EAF_contrasts()`. It will produce an all-by-all table
#' and run through `validate_delta_EAF_contrasts()` to modify the class.
#'
#' @param q a qSIP2 list object
#'
#' @returns a dataframe
#' @export

make_delta_EAF_contrasts = function(q) {

  is_qsip_data_list(q, error = TRUE)

  tibble::tibble(control = names(q)) |>
    tidyr::crossing(treatment = names(q)) |>
    dplyr::filter(control < treatment) |>
    dplyr::mutate(contrast = paste(control, "vs", treatment)) |>
    validate_delta_EAF_contrasts(q = q, contrasts = _)
}



#' Validate a delta EAF contrasts table
#'
#' @param q a qSIP2 list object
#' @param contrasts A validated contrasts table
#'
#' @export
#'
#' @returns a validated contrasts table

validate_delta_EAF_contrasts <- function(q, contrasts) {

  is_qsip_data_list(q, error = TRUE)

  q_names <- names(q)

  # required columns
  req <- c("control", "treatment", "contrast")
  missing <- setdiff(req, names(contrasts))
  if (length(missing)) {
    cli::cli_abort("{.arg contrasts} missing column(s): {missing}")
  }

  # control and treatment must differ
  same <- contrasts$control == contrasts$treatment
  same[is.na(same)] <- FALSE
  if (any(same)) {
    bad_rows <- which(same)
    cli::cli_abort("control and treatment cannot be the same in row(s): {bad_rows}")
  }

  # unique contrast labels
  dup <- contrasts$contrast[duplicated(contrasts$contrast)]
  if (length(dup)) {
    cli::cli_abort("duplicate contrast label(s): {.val {unique(dup)}}")
  }

  # A/B must be in master list (typo check)
  master <- setdiff(q_names, "resample")
  used <- unique(c(contrasts$control, contrasts$treatment))
  bad <- setdiff(used, master)
  if (length(bad)) {
    stop("unknown control/treat,emt name(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }

  # tag the tibble as validated (keep it a tibble)
  class(contrasts) <- c("validated_contrasts", class(contrasts))
  contrasts
}

#' Get the delta distribution for the contrasts from resampling data
#'
#' @param resampled_EAF a tibble
#' @param contrasts A validated contrasts table
#'
#' @returns a list of deltas and messages
#' @keywords internal

get_delta_distributions = function(resampled_EAF, contrasts) {

  # resampled_EAF is a long dataframe with a grouping column, a resample number
  # column, and an EAF value. If the resample is NA it is the TRUE value, not an
  # actual resample


  # makes a wide version of each resampled EAF according to grouping.
  # Importantly, leaves an NA value for features with no NA, and these will be
  # dropped later with the contrasts
  data = resampled_EAF |>
    tidyr::pivot_wider(names_from = group,
                       values_from = EAF)

  # some dataframes won't have all possible rows in the contrasts, so this will
  # remove those rows and make a valid_contrasts tibble
  valid_contrasts <- contrasts |>
    dplyr::filter(control %in% names(data) & treatment %in% names(data))

  # these two steps make a status note keeping track of which contrasts weren't
  # possible for this dataset
  dropped <- nrow(contrasts) - nrow(valid_contrasts)
  status <- dplyr::case_when(
    nrow(valid_contrasts) == 0 ~ "no_valid_contrasts",
    dropped > 0 ~ paste0("skipped ", dropped, " missing contrast(s): ", paste(setdiff(contrasts$contrast, valid_contrasts$contrast), collapse = ", ")),
    TRUE ~ NA
  )

  # because this whole function is being called via purrr::map(), it needs to
  # still return a tibble with the expected columns, even if no contrasts were
  # possible
  delta_distributions <- if (nrow(valid_contrasts) == 0) {
    tibble::tibble(
      resample = data$resample[0],
      contrast = character(),
      delta = numeric()
    )
  } else {
    # but if contrasts are possible, then convert each pairs EAF value to a
    # shared delta EAF of control - treatment, and produce a new 3 column tibble
    # with resample number (NA still means the true delta), the contrast name,
    # and the delta EAF
    purrr::map_dfr(seq_len(nrow(valid_contrasts)), \(i) {
      a <- valid_contrasts$control[i]
      b <- valid_contrasts$treatment[i]
      comp <- valid_contrasts$contrast[i]

      data |>
        dplyr::transmute(
          resample,
          contrast = comp,
          delta = .data[[b]] - .data[[a]] # treatment - control
        )
    })
  }

  # return both the result and the status as a list, and that will be unpacked
  # and mapped to two columns back in the main function
  list(delta_distributions = delta_distributions, status = status)
}

#' Calculate the delta EAF statistics
#'
#' @param delta_distributions a tibble
#' @param confidence (numeric, default: 0.95) confidence interval
#'
#' @keywords internal
#' @returns a tibble of statistics

get_delta_stats = function(delta_distributions, confidence = 0.95) {

  delta_stats <- if (nrow(delta_distributions) == 0) {
    tibble::tibble(
      contrast = character(),
      delta = numeric(),
      lower = numeric(),
      upper = numeric(),
      sd = numeric(),
      bs_pval = numeric(),
      pval = numeric(),
    )
  } else {

    delta = delta_distributions |>
      dplyr::filter(is.na(resample)) |>
      dplyr::select(-resample)

    delta_dist = delta_distributions |>
      dplyr::filter(!is.na(resample)) |>
      dplyr::summarize(lower = quantile(delta, (1 - confidence) / 2, na.rm = T),
                       upper = quantile(delta, 1 - (1 - confidence) / 2, na.rm = T),
                       sd = sd(delta, na.rm = T),
                       .by = contrast)

    if (nrow(delta_distributions) == 0) {
      bs_pval = NULL
    } else {
      bs_pval = delta_distributions |>
        dplyr::filter(!is.na(resample)) |>
        dplyr::summarize(result = list(calculate_bootstrap_pvalue(delta, 0)), # !!! unpacks a list into their names
                  .by = contrast) |>
        tidyr::unnest_wider(result)
    }

    delta |>
      dplyr::left_join(delta_dist, by = dplyr::join_by(contrast)) |>
      dplyr::left_join(bs_pval, by = dplyr::join_by(contrast)) |>
      dplyr::mutate(pval = 2 * stats::pnorm(-abs(delta / sd)))
  }

  return(delta_stats)
}

#' Two-sided p-value from a bootstrap distribution
#'
#' Computes a two-sided p-value for testing whether a parameter equals a
#' specified null value using a bootstrap distribution of an estimator.
#'
#' The p-value is obtained by comparing the null value to the empirical
#' bootstrap distribution and doubling the smaller tail probability
#' (with a half-probability correction for ties). This corresponds to
#' inversion of a percentile bootstrap confidence interval: the p-value
#' is small when the null value lies in the tails of the bootstrap
#' distribution and large when it lies in the central region.
#'
#' Note that this is not a parametric test and does not simulate data under
#' the null hypothesis. Instead, it evaluates whether the null value is
#' compatible with the sampling uncertainty implied by the bootstrap
#' distribution of the statistic.
#'
#' Bootstrap replicates equal to the null value contribute half to the lower
#' tail probability and half to the upper tail probability.
#'
#' @param bs_distribution Numeric vector of bootstrap replicates of a statistic.
#' @param null_value Numeric scalar giving the null value to test against.
#'
#' @return A list with a p-value and a message
#'
#' @keywords internal

calculate_bootstrap_pvalue <- function(bs_distribution, null_value = 0) {

  # make sure bs_distributions contains only numbers
  if (!is.numeric(bs_distribution) || !is.vector(bs_distribution)) {
    stop("<bs_distribution> must be a numeric vector.", call. = FALSE)
  }
  # make sure null_value only a single number
  if (!is.numeric(null_value) || length(null_value) != 1L || is.na(null_value)) {
    stop("<null_value> must be a numeric value.", call. = FALSE)
  }

  n0 <- length(bs_distribution)
  if (n0 == 0L) cli::cli_abort("{.arg bs_distribution} must not be empty.")

  n_na <- sum(is.na(bs_distribution))
  if (n_na > 0L) {
    bs_distribution <- bs_distribution[!is.na(bs_distribution)]
    bs_pval_message <- cli::format_message("Removed {n_na} NA bootstrap replicate(s) of {n0} ({round(100 * n_na / n0, 1)}%)") |>
      cli::ansi_strip()
  } else {
    bs_pval_message = NULL
  }

  p_eq <- mean(bs_distribution == null_value)
  p_lo <- mean(bs_distribution <  null_value) + 0.5 * p_eq
  p_hi <- mean(bs_distribution >  null_value) + 0.5 * p_eq

  list("bs_pval" = min(1, 2 * min(p_lo, p_hi)),
       "bs_pval_message" = bs_pval_message
  )
}

#' Return the count of features present between different groups
#'
#' @param q a qSIP2 list object
#'
#' @export
#' @returns a tibble

get_overlap_sizes = function(q) {

  df = lapply(q, get_EAF_data) |>
    dplyr::bind_rows(.id = "group") |>
    dplyr::filter(is.na(resample)) |>
    dplyr::select(group, feature_id) |>
    dplyr::distinct(group, feature_id) # don’t double-count

  # cant pass two underscores, so have to save as df and pass twice like this
  dplyr::inner_join(x = df, # all group pairs per feature
                    y = df,
                    by = "feature_id",
                    relationship = "many-to-many") |>
    dplyr::filter(group.x < group.y) |> # keep each pair once; drop self-pairs
    dplyr::count(group1 = group.x, group2 = group.y, name = "overlapping_features")

}
