#' Plot growth values
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object
#' @param confidence (*numeric*) The confidence level for the confidence interval
#' @param top (*numeric*) The number of top features to plot. Use `Inf` for all
#' @param error (*character*) The type of error bars to plot. Options are 'none', 'bar', 'ribbon'
#' @param alpha (*numeric*) The transparency of the error bar/ribbon
#' @param type (*character*) The type of growth values to plot. Options are 'rates' or "copies
#'
#' @export

plot_growth_values <- function(qsip_data_object,
                               confidence = 0.9,
                               top = Inf,
                               error = "none",
                               alpha = 0.4,
                               type = "rates") {


  # bind variables
  observed_ri <- feature_id <- resampled_ri_mean <- timepoint1 <- timepoint2 <- N_total_i0 <- N_total_it <- r_net <- rate <- observed <- lower <- upper <- resampled_N_mean <- stat <- NULL

  rbd <- summarize_growth_values(qsip_data_object, confidence = confidence)

  palette <- c(
    "ri" = "cornflowerblue",
    "bi" = "seagreen4",
    "di" = "tomato"
  )

  # rates
  rbd_rate <- rbd |>
    dplyr::slice_max(observed_ri, n = top) |>
    dplyr::mutate(feature_id = forcats::fct_reorder(feature_id, resampled_ri_mean)) |>
    tidyr::pivot_longer(
      cols = c(dplyr::everything(), -feature_id, -timepoint1, -timepoint2, -N_total_i0, -N_total_it, -r_net),
      names_to = "rate",
      values_to = "value"
    ) |>
    tidyr::separate(rate,
                    into = c("observed", "rate", "stat"),
                    sep = "_",
                    fill = "right"
    ) |>
    dplyr::filter(rate %in% c("ri", "bi", "di")) |>
    dplyr::mutate(stat = ifelse(is.na(stat), "observed", stat)) |>
    dplyr::filter(stat != "sd") |>
    dplyr::select(-observed) |>
    tidyr::pivot_wider(names_from = "stat", values_from = "value") |>
    dplyr::select(-observed)

  p_rate <- rbd_rate |>
    ggplot2::ggplot(ggplot2::aes(y = feature_id, x = mean, color = rate)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette)

  if (error == "bar") {
    p_rate <- p_rate + ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper),
                                              width = 0.2,
                                              alpha = alpha
    )
  } else if (error == "ribbon") {
    p_rate <- p_rate +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = lower, xmax = upper, group = rate, fill = rate), alpha = alpha)
  }


  # N copies
  p_N <- rbd |>
    dplyr::slice_max(N_total_it, n = top) |>
    dplyr::mutate(feature_id = forcats::fct_reorder(feature_id, N_total_it)) |>
    dplyr::select(feature_id, N_total_i0, N_total_it, r_net, resampled_N_mean) |>
    ggplot2::ggplot(ggplot2::aes(y = feature_id, x = (N_total_i0))) +
    # ggplot2::geom_errorbar(ggplot2::aes(xmin = (resampled_N_lower), xmax = (resampled_N_upper)),
    #   width = 0.5,
    #   linewidth = 1,
    #   color = "orangered3",
    #   alpha = alpha
    # ) +
    ggplot2::geom_segment(ggplot2::aes(x = N_total_i0, xend = N_total_it),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                          color = "gray50"
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(x = resampled_N_mean), color = "orangered3") +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_x_continuous(
      labels = function(x) {
        format(x,
               big.mark = ",",
               decimal.mark = ".",
               scientific = FALSE
        )
      },
      trans = "log2"
    )

  if (type == "rates") {
    p_rate
  } else if (type == "copies") {
    p_N
  } else {
    stop("type must be either 'rates' or 'copies'")
  }
}


#' Plot qSIP sample data density curves
#'
#' @param qsip_data_object (*qsip_data*) qSIP object
#' @param title (*character*) An optional title for the plot
#' @param facet_by (*character*) Facet the plots by "source" or by "isotope"
#' @param show_wad (*logical*) A logical value
#' @param colors deprecated
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_sample_curves <- function(qsip_data_object,
                               title = NULL,
                               facet_by = "source",
                               show_wad = FALSE,
                               colors = lifecycle::deprecated()) {
  is_qsip_data(qsip_data_object, error = TRUE)

  # error if title is not a string
  if (!is.null(title) && !is.character(title)) {
    stop("title must be a character string", call. = FALSE)
  }

  # facet_by must be either "source" or "isotope"
  if (!facet_by %in% c("source", "isotope")) {
    stop("facet_by must be either 'source' or 'isotope'", call. = FALSE)
  }

  # show_wad must be boolean
  if (!is.logical(show_wad)) {
    stop("show_wad must be TRUE/FALSE", call. = F)
  }

  if (lifecycle::is_present(colors)) {
    lifecycle::deprecate_warn("0.18.3", "plot_sample_curves(colors)")
  }

  # bind variables
  sample_id <- gradient_position <- source_mat_id <- isotope <- WAD <- gradient_pos_density <- gradient_pos_rel_amt <- NULL

  df <- qsip_data_object@tube_rel_abundance |>
    dplyr::left_join(
      qsip_data_object@sample_data@data |>
        dplyr::select(sample_id, gradient_position),
      by = "sample_id"
    ) |>
    dplyr::left_join(
      qsip_data_object@source_data@data |>
        dplyr::select(source_mat_id, isotope),
      by = "source_mat_id"
    )

  if (any(df$gradient_position == -1)) {
    message("some unfractionated samples have been filtered from this plot")
  }

  df <- df |>
    dplyr::filter(gradient_position > 0) |>
    dplyr::summarize(tube_rel_abundance = sum(tube_rel_abundance), .by = c(sample_id, source_mat_id, gradient_position, gradient_pos_density, isotope))

  source_wads <- qsip_data_object@source_wads |>
    dplyr::filter(!is.na(WAD)) |>
    dplyr::left_join(qsip_data_object@source_data@data, by = "source_mat_id")

  p <- df |>
    dplyr::filter(!is.na(gradient_position)) |>
    # dplyr::filter(gradient_pos_density > 1.5) |>
    # dplyr::group_by(source_mat_id) |>
    ggplot2::ggplot(ggplot2::aes(
      x = gradient_pos_density,
      y = tube_rel_abundance
    ))

  if (facet_by == "source") {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = isotope)) +
      ggplot2::geom_line(linewidth = 1, ggplot2::aes(color = isotope)) +
      ggplot2::facet_wrap(~source_mat_id, scales = "free_y") +
      ggplot2::scale_color_manual(values = isotope_palette)


    if (isTRUE(show_wad)) {
      p <- p +
        ggplot2::geom_vline(
          data = source_wads,
          size = 1,
          linetype = 2, ggplot2::aes(
            xintercept = WAD,
            color = isotope
          )
        )
    }
  } else if (facet_by == "isotope") {
    p <- p +
      ggplot2::geom_point(ggplot2::aes(color = source_mat_id)) +
      ggplot2::geom_line(linewidth = 1, ggplot2::aes(group = source_mat_id, color = source_mat_id)) +
      ggplot2::facet_wrap(~isotope) +
      ggplot2::scale_color_manual(values = source_palette(nrow(source_wads)))

    if (isTRUE(show_wad)) {
      p <- p +
        ggplot2::geom_vline(
          data = source_wads,
          size = 1,
          linetype = 2, ggplot2::aes(
            xintercept = WAD,
            color = source_mat_id
          )
        )
    }
  }

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}



#' Plot the source WADs by isotope
#'
#' @param qsip_data (*qsip_data*) qSIP object
#' @param group (*character*) An optional grouping parameter to facet the y or x,y axes
#' @param title (*character*) An optional title for the plot
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_source_wads <- function(qsip_data,
                             group = NULL,
                             title = NULL) {
  is_qsip_data(qsip_data, error = TRUE)

  # bind variables
  WAD <- isotope <- NULL

  source_data <- qsip_data@source_data
  sample_data <- qsip_data@sample_data

  # error if group is not a column name in source_data
  if (!is.null(group) && !group %in% colnames(source_data@data)) {
    stop("group must be a column name in source_data", call. = FALSE)
  }

  p <- qsip_data@source_wads |>
    dplyr::filter(!is.na(WAD)) |> # filter unfractionated
    dplyr::left_join(source_data@data, by = "source_mat_id") |>
    ggplot2::ggplot(ggplot2::aes(color = isotope)) +
    ggplot2::geom_segment(y = 0, yend = 1, ggplot2::aes(x = WAD, xend = WAD), linewidth = 1) +
    ggplot2::facet_grid(paste(group, " ~ .", sep = "")) +
    ggplot2::scale_color_manual(values = isotope_palette) +
    ggplot2::labs(x = "Weighted Average Density")

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}


#' Cook's outlier detection on gradient positions vs densities
#'
#' Assuming a linear relationship between the gradient_position and gradient_pos_density,
#' this function will plot any potential outliers using Cook's distance and a defined
#' sensitivity. The lower the sensitivity, the more likely outliers will be flagged.
#'
#' @param sample_data (*qsip_sample_data or qsip_data*) A qsip object with sample data
#' @param sensitivity (*numeric, default: 4*) A sensitivity value, with lower values being more sensitive to outlier detection
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"


plot_density_outliers <- function(sample_data,
                                  sensitivity = 4) {
  if (inherits(sample_data, qsip_data)) {
    data <- sample_data@sample_data@data
  } else if (inherits(sample_data, qsip_sample_data)) {
    data <- sample_data@data
  } else {
    stop(glue::glue("sample_data should be class <qsip_sample_data> or <qsip_data>, not {class(sample_data)[1]}"), call. = FALSE)
  }

  stopifnot("sensitivity should be a <numeric>" = is.numeric(sensitivity))

  if (any(data$gradient_position == -1)) {
    message("some unfractionated samples have been filtered from this plot")
  }

  # bind variables
  gradient_position <- source_mat_id <- . <- .cooksd <- COOKS_CUTOFF <- gradient_pos_density <- sample_id <- NULL


  data <- data |>
    dplyr::filter(gradient_position > 0)

  S <- data |>
    dplyr::group_by(source_mat_id) |>
    dplyr::do(broom::augment(stats::lm(gradient_pos_density ~ gradient_position, data = .))) |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_"), COOKS_CUTOFF = sensitivity / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::select(S, .cooksd, COOKS_CUTOFF)

  potential_outliers <- S |>
    dplyr::filter(.cooksd >= COOKS_CUTOFF) |>
    dplyr::pull(S)

  data |>
    dplyr::mutate(S = paste(source_mat_id, gradient_position, sep = "_")) |>
    dplyr::left_join(S, by = "S") |>
    ggplot2::ggplot(ggplot2::aes(x = gradient_position, y = gradient_pos_density)) +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ x") +
    ggplot2::geom_point(ggplot2::aes(color = ifelse(.cooksd > COOKS_CUTOFF, "red", "gray30"))) +
    ggrepel::geom_text_repel(
      verbose = FALSE,
      na.rm = TRUE,
      size = 3,
      color = "gray30",
      ggplot2::aes(label = ifelse(.cooksd > COOKS_CUTOFF, sample_id, NA))
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::facet_wrap(~source_mat_id) +
    ggplot2::labs(
      title = "Cook's Distance Outlier Detection",
      subtitle = glue::glue("sensitivity = {sensitivity}")
    )
}



#' Plot EAF and confidence intervals
#'
#' This function plots the observed EAF values for each feature in the dataset.
#' The features are ordered by their observed EAF values. The confidence intervals
#' are plotted as error bars or ribbons.
#'
#' Either a single qsip object or a list of named qsip objects can be passed to this
#' function. If giving a list of qsip objects the plot will be faceted by the
#' list names.
#'
#' If the resampling step was run with the default `allow_failures = FALSE`, then the
#' points will just be colored a generic blue. But, if resampling was instead run
#' with `allow_failures = TRUE`, then the points are colored based on the success
#' ratio of the resamples. If giving a list of qsip objects then the pass/fail color scheme
#' will be applied if any of the qsip objects have `allow_failures = TRUE`.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object or list of qsip_data objects
#' @param confidence (*numeric*) The confidence level for the confidence interval
#' @param success_ratio (*numeric*) The ratio of successful resamples to total resamples
#' @param top (*numeric*) The number of top features to plot. Use `Inf` for all
#' @param error (*character*) The type of error bars to plot. Options are 'none', 'bar', 'ribbon'
#' @param alpha (*numeric*) The transparency of the error bar/ribbon
#' @param zero_line (*logical*) Add a line at EAF = 0
#' @param shared_y (*logical*) Use a shared y-axis for the facets
#' @param title (*character*) An optional title of the plot
#' @param taxonomy (*logical*) If TRUE, the taxonomy will be added to the plot
#'
#' @export

plot_EAF_values <- function(qsip_data_object,
                            confidence = 0.9,
                            success_ratio = 0.9,
                            top = Inf,
                            error = "none",
                            alpha = 0.3,
                            zero_line = TRUE,
                            shared_y = FALSE,
                            title = NULL,
                            taxonomy = NULL) {
  # confirm qsip_data_object class is either qsip_data or list

  if (is_qsip_data_list(qsip_data_object, error = FALSE)) {
    object_type <- "multiple"
  } else if (is_qsip_data(qsip_data_object, error = FALSE)) {
    object_type <- "single"
  } else {
    stop("ERROR: qsip_data_object must be of class <qsip_data> or <list> of qsip_data objects")
  }

  # confirm the confidence value is numeric and between 0-1
  stopifnot("ERROR: confidence should be numeric" = is.numeric(confidence))
  if (confidence >= 1 | confidence <= 0) {
    stop("ERROR: confidence level should be between 0 and 1")
  }

  # confirm the success_ratio value is numeric and between 0-1
  stopifnot("ERROR: success_ratio should be numeric" = is.numeric(success_ratio))
  if (success_ratio > 1 | success_ratio <= 0) {
    stop("ERROR: success_ratio should be between 0 and 1")
  }

  # confirm the alpha value is numeric and between 0-1
  stopifnot("ERROR: alpha should be numeric" = is.numeric(alpha))
  if (alpha > 1 | alpha <= 0) {
    stop("ERROR: alpha level should be between 0 and 1")
  }

  if (!error %in% c("none", "bar", "ribbon")) {
    stop(glue::glue("<error> should be 'none', 'bar' or 'ribbon', not {error}"), call. = FALSE)
  }

  # print warning that data may be missing if shared_y = TRUE and top != NULL
  if (isTRUE(shared_y) & top < Inf) {
    warning("When setting <shared_y> to TRUE and also passing a value to <top>, there will likely be missing data in the plots if a feature is in the top n of one comparison, but not in the other.", call. = FALSE)
  }



  # bind variables
  group <- observed_EAF <- feature_id <- labeled_resamples <- unlabeled_resamples <- resamples <- lower <- upper <- NULL


  if (is.null(taxonomy)) {
    EAF <- summarize_EAF_values(qsip_data_object,
                                confidence = confidence
    )
  } else {
    EAF <- summarize_EAF_values(qsip_data_object,
                                taxonomy = TRUE,
                                confidence = confidence)

    EAF = EAF |>
      dplyr::select(-feature_id) |>
      dplyr::rename(feature_id = all_of(taxonomy))
  }



  # add number of attempted resamples
  if (object_type == "multiple") {
    EAF <- EAF |>
      dplyr::group_by(group) |>
      dplyr::slice_max(observed_EAF, n = top) |>
      dplyr::ungroup()

    if (isFALSE(shared_y)) {
      EAF <- EAF |>
        dplyr::mutate(feature_id = tidytext::reorder_within(feature_id, observed_EAF, within = group))
    }

    EAF <- EAF |>
      dplyr::left_join(
        sapply(qsip_data_object, n_resamples) |>
          tibble::enframe(name = "group", value = "resamples"),
        by = "group"
      )
  } else {
    EAF <- EAF |>
      dplyr::slice_max(observed_EAF, n = top) |>
      dplyr::mutate(resamples = qsip_data_object@resamples$n)
  }





  p <- EAF |>
    dplyr::mutate(feature_id = forcats::fct_reorder(feature_id, observed_EAF)) |>
    ggplot2::ggplot(ggplot2::aes(y = feature_id, x = observed_EAF)) +
    ggplot2::geom_point(
      pch = 21,
      size = 2,
      ggplot2::aes(fill = ifelse((labeled_resamples + unlabeled_resamples) > resamples * 2 * success_ratio,
                                 "Passed",
                                 "Failed"
      ))
    )

  # color and label differently depending on allow_failures
  ## first, make variable for if any of the qsip objects have allow_failures true
  if (object_type == "multiple") {
    allow_failures <- any(sapply(qsip_data_object, function(x) x@resamples$allow_failures))
  } else if (object_type == "single") {
    allow_failures <- qsip_data_object@resamples$allow_failures
  } else {
    allow_failures <- FALSE
  }

  if (isTRUE(allow_failures)) {
    p <- p +
      ggplot2::scale_fill_manual(values = c("Passed" = "#00ff0066", "Failed" = "red")) +
      ggplot2::labs(
        fill = glue::glue(">{success_ratio * 100}% successes"),
        y = "feature_ids reordered by observed_EAF"
      )
  } else {
    p <- p +
      ggplot2::scale_fill_manual(values = c("Passed" = "#037bcf", "Failed" = "#037bcf")) +
      ggplot2::labs(
        y = "feature_ids reordered by observed_EAF"
      ) +
      ggplot2::guides(fill = "none")
  }

  # control the CI display
  if (error == "bar") {
    p <- p + ggplot2::geom_errorbar(ggplot2::aes(xmin = lower, xmax = upper), alpha = alpha)
  } else if (error == "ribbon") {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = lower, xmax = upper, group = 1), alpha = alpha)
  }

  if (isTRUE(zero_line)) {
    p <- p +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "black")
  }

  if (object_type == "multiple") {
    if (isFALSE(shared_y)) {
      p <- p +
        tidytext::scale_y_reordered() +
        ggplot2::facet_wrap(~group, scales = "free_y")
    } else {
      p <- p +
        ggplot2::facet_wrap(~group)
    }
  }

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  return(p)
}


#' Plot qSIP feature data density curves
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object with tube relative abundances
#' @param feature_ids (*string*) Feature ids to be plotted on their own facet
#' @param source_mat_ids (*string, defaults to all*) A list of source material ids
#' @param title (*string*) An optional title for the plot
#'
#' @export
#'
#' @returns A ggplot object
#'
#' @family "visualizations"

plot_feature_curves <- function(qsip_data_object,
                                feature_ids,
                                source_mat_ids = NULL,
                                title = NULL) {
  is_qsip_data(qsip_data_object, error = TRUE)

  # make sure all values in source_mat_ids are found in get_source_mat_ids(qsip_data_object)
  if (!is.null(source_mat_ids)) {
    if (length(setdiff(source_mat_ids, get_source_mat_ids(qsip_data_object))) > 0) {
      stop("some provided source_mat_ids are not found in the qsip_data object", call. = F)
    }
  }


  # get all source_mat_ids if NULL
  if (is.null(source_mat_ids)) {
    source_mat_ids <- qsip_data_object@source_data@data$source_mat_id
  }


  if (length(setdiff(feature_ids, qsip_data_object@feature_data@data$feature_id)) > 0) {
    stop("some provided feature_ids are not found in the qsip_data object", call. = F)
  }

  # bind variables
  source_mat_id <- isotope <- feature_id <- gradient_pos_density <- tube_rel_abundance <- NULL

  # get source data for isotope
  s_data <- qsip_data_object@source_data@data |>
    dplyr::select(source_mat_id, isotope)

  p <- qsip_data_object@tube_rel_abundance |>
    dplyr::left_join(s_data, by = "source_mat_id") |>
    dplyr::filter(source_mat_id %in% source_mat_ids) |>
    dplyr::filter(feature_id %in% feature_ids) |>
    ggplot2::ggplot(ggplot2::aes(
      x = gradient_pos_density,
      y = tube_rel_abundance,
      color = isotope
    )) +
    ggplot2::geom_line(ggplot2::aes(group = source_mat_id)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~feature_id, scales = "free_y") +
    ggplot2::scale_color_manual(values = c(
      "12C" = "#037bcf", "13C" = "#ff0000",
      "14C" = "#037bcf", "15N" = "#ff0000",
      "16O" = "#037bcf", "18O" = "#ff0000"
    ))

  if (!is.null(title)) {
    p <- p + ggplot2::labs(title = title)
  }

  p
}




#' Plot occurrence of features in samples
#'
#' This is a plotting function to visualize the occurrence of features in samples.
#' The function takes a qsip_data object and a vector of feature_ids, and can scale
#' the results by total abundance or source abundance, and the WAD value can also be shown.
#'
#' @param qsip_data_object (*qsip_data*) A qsip_data object
#' @param feature_ids (*character*) An optional vector of feature_ids
#' @param scale (*character*) A character string
#' @param show_wad (*logical*) A logical value
#' @param title (*character*) A character string
#' @param legend.position (*character* or *numeric vector*) Values passed to ggplot2::theme(legend.position = ...)
#'
#' @export
#'
#' @returns Returns a ggplot object


plot_feature_occurrence <- function(qsip_data_object,
                                    feature_ids = NULL,
                                    scale = "none",
                                    show_wad = FALSE,
                                    title = NULL,
                                    legend.position = "right") {
  # stop if qsip_data_object !is_qsip()
  if (!is_qsip_data(qsip_data_object)) {
    stop("qsip_data_object should be class <qsip_data>", call. = F)
  }

  # feature_ids must be null or a vector of strings
  if (!is.null(feature_ids) & !is.character(feature_ids)) {
    stop("<feature_ids> argument must be NULL or a vector of strings", call. = FALSE)
  }

  # scale must be "none", "total", or "source"
  if (!scale %in% c("none", "total", "source")) {
    stop("scale must be 'none', 'total', or 'source'", call. = F)
  }

  # show_wad must be boolean
  if (!is.logical(show_wad)) {
    stop("show_wad must be a boolean", call. = F)
  }

  # title must be a character or NULL
  if (!is.character(title) & !is.null(title)) {
    stop("title must be a character or NULL", call. = F)
  } else if (length(title) > 1) {
    stop("title should only have a length one 1", call. = F)
  }

  # if feature_ids is null, then get all
  if (is.null(feature_ids)) {
    feature_ids <- get_feature_ids(qsip_data_object, filtered = T)
  }

  # if the sample_data contains an isotope column, remove it so there isn't a .x, .y
  sample_df <- qsip_data_object@sample_data@data |> dplyr::select(-dplyr::any_of("isotope"))

  # make dataframe with joined metadata
  df <- qsip_data_object@tube_rel_abundance |>
    dplyr::filter(feature_id %in% feature_ids) |>
    dplyr::left_join(sample_df,
                     by = dplyr::join_by(sample_id, source_mat_id, gradient_pos_density, gradient_pos_rel_amt)
    ) |>
    dplyr::left_join(qsip_data_object@source_data@data,
                     by = dplyr::join_by(source_mat_id)
    ) |>
    dplyr::left_join(qsip_data_object@wads,
                     by = dplyr::join_by(feature_id, source_mat_id)
    ) |>
    dplyr::mutate(isotope = as.factor(isotope)) |>
    dplyr::mutate(source_mat_id = forcats::fct_reorder(source_mat_id, as.numeric(isotope)))

  # if scale = "source"
  if (scale == "source") {
    df <- df |>
      dplyr::mutate(
        tube_rel_abundance_source = tube_rel_abundance / sum(tube_rel_abundance),
        .by = c("feature_id", "source_mat_id")
      )
  }

  # base plot
  p <- df |>
    ggplot2::ggplot(ggplot2::aes(y = source_mat_id))

  # if show_wad is true
  if (isTRUE(show_wad)) {
    p <- p +
      ggplot2::geom_point(
        pch = "|", size = 6,
        ggplot2::aes(x = WAD, color = isotope)
      ) +
      ggplot2::scale_color_manual(values = isotope_palette)
  }

  # choose which size to plot based on scale argument
  if (scale == "total") {
    p <- p +
      ggplot2::geom_point(
        pch = 21, alpha = 0.9,
        ggplot2::aes(x = gradient_pos_density, size = tube_rel_abundance, fill = isotope)
      )
  } else if (scale == "source") {
    p <- p +
      ggplot2::geom_point(
        pch = 21, alpha = 0.9,
        ggplot2::aes(x = gradient_pos_density, size = tube_rel_abundance_source, fill = isotope)
      )
  } else {
    p <- p +
      ggplot2::geom_point(
        pch = 21, alpha = 0.9, size = 2,
        ggplot2::aes(x = gradient_pos_density, fill = isotope)
      )
  }


  # finish and return plot
  p +
    ggplot2::facet_wrap(~feature_id) +
    ggplot2::scale_fill_manual(values = isotope_palette) +
    ggplot2::labs(title = title) +
    ggplot2::theme(legend.position = legend.position) +
    ggplot2::scale_y_discrete(limits = rev)
}




#' Plot the resampled EAFs for each feature
#'
#' This plot will show the results of the resampling procedure for each feature.
#' The resampling procedure is run using the run_resampling() function. The plot
#' will show the mean resampled EAF for each feature, with the confidence interval
#' (default 90%) shown as a bar or line (default no line). The area under the curve
#' can also be shown (default TRUE).
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled
#' @param feature_ids (*character vector*) A vector of feature ids to filter on
#' @param area (*boolean*) Whether to plot the area under the curve or not (default: TRUE)
#' @param confidence (*numeric*) The confidence interval to plot
#' @param intervals (*character*) Whether to plot the confidence interval as a bar, line or not at all (default)
#'
#' @export
#'
#' @returns A ggplot object

plot_feature_resamplings <- function(qsip_data_object,
                                     feature_ids = NULL,
                                     area = TRUE,
                                     confidence = 0.9,
                                     intervals = "") {
  if (isFALSE(is_qsip_resampled(qsip_data_object, error = FALSE))) {
    stop("This function requires a qsip object that has been run through run_resampling()", call. = FALSE)
  }

  # feature_ids must be null or a vector of strings
  if (!is.null(feature_ids) & !is.character(feature_ids)) {
    stop("<feature_ids> argument must be NULL or a vector of strings", call. = FALSE)
  }

  # area must be true or false
  if (!isTRUE(area) & !isFALSE(area)) {
    stop("<area> argument must be TRUE or FALSE", call. = FALSE)
  }

  # confidence must be between 0 and including 1
  if (confidence <= 0 | confidence > 1) {
    stop("<confidence> argument must be between 0 and 1", call. = FALSE)
  }

  # error if interval is not "", "bar" or "line"
  if (intervals != "" & intervals != "bar" & intervals != "line") {
    stop("<intervals> argument must be 'bar' or 'line'", call. = FALSE)
  }

  # bind variables
  value <- feature_id <- resample <- type <- mean_resampled_WAD <- mean_resampled_WAD2 <- lower <- upper <- NULL

  unlabeled_data <- dplyr::bind_rows(qsip_data_object@resamples$u) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("unlabeled_")) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::summarize(mean_resampled_WAD = mean(value), .by = c(feature_id, resample, type))

  labeled_data <- dplyr::bind_rows(qsip_data_object@resamples$l) |>
    tidyr::pivot_longer(cols = dplyr::starts_with("labeled_")) |>
    dplyr::filter(!is.na(value)) |>
    dplyr::summarize(mean_resampled_WAD = mean(value), .by = c(feature_id, resample, type))

  combined_data <- rbind(unlabeled_data, labeled_data)

  # filter feature ids is given
  if (!is.null(feature_ids)) {
    # if no overlap between feature_ids and combined_data$feature_data give error
    if (length(intersect(feature_ids, combined_data$feature_id)) == 0) {
      stop("None of the features in feature_ids were found in the <qsip_data_object>", call. = FALSE)
    }


    # print a message if not all of the feature_ids are found in combined_data$feature_id
    if (length(setdiff(feature_ids, combined_data$feature_id)) > 0) {
      message("Some of the features in feature_ids were not found in the <qsip_data_object>")
    }

    combined_data <- combined_data |>
      dplyr::filter(feature_id %in% feature_ids)
  }

  p <- combined_data |>
    ggplot2::ggplot(ggplot2::aes(x = mean_resampled_WAD, y = type)) +
    ggplot2::facet_wrap(~feature_id, scales = "free_x") +
    ggplot2::scale_color_manual(values = c("labeled" = "#ff0000", "unlabeled" = "#037bcf")) +
    ggplot2::scale_fill_manual(values = c("labeled" = "#FF000055", "unlabeled" = "#037bcf55")) +
    ggplot2::labs(x = "Resampled WAD Values")

  if (isTRUE(area)) {
    p <- p +
      ggridges::geom_density_ridges(ggplot2::aes(fill = type, height = ggplot2::after_stat(density)), stat = "density")
  }

  # options to add confidence interval data
  summary_statistics <- combined_data |>
    dplyr::group_by(feature_id, type) |>
    dplyr::summarize(
      mean_resampled_WAD2 = mean(mean_resampled_WAD),
      lower = stats::quantile(mean_resampled_WAD, (1 - confidence) / 2, na.rm = T),
      upper = stats::quantile(mean_resampled_WAD, 1 - (1 - confidence) / 2, na.rm = T),
      .groups = "drop"
    ) |>
    dplyr::rename(mean_resampled_WAD = mean_resampled_WAD2)

  if (intervals == "bar") {
    p <- p +
      ggplot2::geom_errorbarh(data = summary_statistics, ggplot2::aes(xmin = lower, xmax = upper, color = type), linewidth = 1, show.legend = F)
  } else if (intervals == "line") {
    p <- p +
      ggplot2::geom_vline(data = summary_statistics, ggplot2::aes(xintercept = mean_resampled_WAD, color = type), show.legend = F) +
      ggplot2::geom_vline(data = summary_statistics, ggplot2::aes(xintercept = lower, color = type), linetype = 2, show.legend = F) +
      ggplot2::geom_vline(data = summary_statistics, ggplot2::aes(xintercept = upper, color = type), linetype = 2, show.legend = F)
  }

  p
}





#' Plot the results of filtering features
#'
#' After running the `run_feature_filter()` code, this function will produce two
#' plot detailing the consequences of the filtering. Plot A shows the retained and
#' removed features by their total tube relative abundance contribution, and plot B
#' shows the retained and removed features by the total count of each category. The
#' "zero fractions" shown in plot B are those entirely missing from the given source_mat_id
#' and are thus not going to be retained no matter what the `run_filter_feature()`
#' parameters are.
#'
#' @param qsip_data_object (*qsip_data*)
#' @param return_type (*string, default: combined*) Changes the return type from a combined plot (*combined*), list of individual plots (*individual*) or list of dataframes (*dataframe*)
#' @param colors deprecated
#'
#' @export
#'
#' @family "visualizations"
#'
#' @return Combined or individual plots, or the raw dataframes


plot_filter_results <- function(qsip_data_object,
                                return_type = "combined",
                                colors = lifecycle::deprecated()) {

  is_qsip_filtered(qsip_data_object, error = TRUE)

  if (lifecycle::is_present(colors)) {
    lifecycle::deprecate_warn("0.17.10", "plot_filter_results(colors)")
  }

  if (!return_type %in% c("combined", "individual", "dataframe")) {
    stop(glue::glue("return_type should be either one of 'combined', 'individual' or 'dataframe'"))
  }

  # bind variables
  source_mat_id <- fraction_call <- tube_rel_abundance <- n <- NULL

  colors <- c(
    "Fraction Passed" = "#037bcf",
    "Fraction Filtered" = "#BE572B",
    "Zero Fractions" = "gray70"
  )

  by_abundance_df <- qsip_data_object@filter_results$fraction_filtered |>
    dplyr::group_by(source_mat_id, fraction_call) |>
    dplyr::summarize(
      tube_rel_abundance = sum(tube_rel_abundance),
      .groups = "drop"
    ) |>
    dplyr::filter(fraction_call != "Zero Fractions") |>
    dplyr::left_join(get_dataframe(qsip_data_object, type = "source"), by = dplyr::join_by(source_mat_id))

  by_abundance <- by_abundance_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = source_mat_id,
      y = tube_rel_abundance,
      fill = fraction_call
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(tube_rel_abundance, accuracy = 0.1)),
                       position = ggplot2::position_stack(vjust = .5),
                       angle = 90
    ) +
    ggplot2::labs(
      x = "source_mat_id",
      y = "Tube Relative Abundance",
      fill = "Filtering Results"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::scale_y_continuous(
      labels = scales::percent,
      breaks = c(0, .25, .50, .75, 1.00)
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90, hjust = 1,
      vjust = 0.5
    )) +
    ggplot2::facet_grid(~isotope, scales = "free_x", space = "free_x")


  by_count_df <- qsip_data_object@filter_results$fraction_filtered |>
    dplyr::group_by(source_mat_id, fraction_call) |>
    dplyr::tally() |>
    dplyr::arrange(fraction_call) |>
    dplyr::left_join(get_dataframe(qsip_data_object, type = "source"), by = dplyr::join_by(source_mat_id))

  by_count <- by_count_df |>
    ggplot2::ggplot(ggplot2::aes(
      x = source_mat_id,
      y = n,
      fill = fraction_call
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = n),
                       position = ggplot2::position_stack(vjust = .5),
                       angle = 90
    ) +
    ggplot2::labs(
      x = "source_mat_id",
      y = "Feature Count",
      fill = "Filtering Results"
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90, hjust = 1,
      vjust = 0.5
    )) +
    ggplot2::facet_grid(~isotope, scales = "free_x", space = "free_x")

  if (return_type == "combined") {
    patchwork::wrap_plots(
      A = (by_abundance +
             ggplot2::theme(legend.position = "none")), # remove legend from A only in patchwork
      B = by_count
    ) +
      patchwork::plot_layout(guides = "collect") +
      patchwork::plot_annotation(
        tag_levels = "A",
        title = "Filtered features",
        subtitle = "A) By tube abundance, B) By feature count"
      )
  } else if (return_type == "individual") {
    list(
      "A" = by_abundance,
      "B" = by_count
    )
  } else if (return_type == "dataframe") {
    list(
      "A" = by_abundance_df,
      "B" = by_count_df
    )
  }
}




plot_filter_gradient_position <- function(qsip_data_object,
                                          return_type = "combined",
                                          colors = NULL) {
  lifecycle::deprecate_warn("0.17.9",
                            "plot_filter_gradient_position()",
                            "plot_filter_results()")
}




#' Plot resampling convergence (under construction!)
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled

plot_resampling_convergence <- function(qsip_data_object) {
  message(glue::glue_col("{red This is an alpha function and is undergoing testing!}"))

  # bind variables
  mean_resampled_EAF <- lower <- upper <- L <- U <- n <- value <- name <- NULL

  k <- purrr::map(
    c(rep(1, 10), rep(2, 10), rep(4, 10), rep(8, 10), rep(16, 10), rep(32, 10), rep(64, 10), rep(128, 10), rep(256, 10), rep(512, 10), rep(1024, 10)),
    \(i) run_resampling(qsip_data_object,
                        resamples = i,
                        # with_seed = 17,
                        progress = FALSE,
                        allow_failures = TRUE,
                        quiet = T
    ) |>
      run_EAF_calculations() |>
      summarize_EAF_values(quiet = T) |>
      dplyr::mutate(n = i),
    .progress = TRUE
  )

  dplyr::bind_rows(k) |>
    ggplot2::ggplot(ggplot2::aes(x = unlabeled_resamples, y = mean_resampled_EAF)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~feature_id, scales = "free_y") +
    ggplot2::scale_x_log10() +
    ggplot2::geom_smooth(
      color = "#037bcf",
      formula = "y ~ x",
      method = "loess"
    ) +
    # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
    ggplot2::geom_hline(aes(yintercept = observed_EAF), color = "red") +
    ggplot2::geom_boxplot(ggplot2::aes(group = unlabeled_resamples))
}





#' Plot the number of successful resamples for each feature_id
#'
#' This function will plot the number of successful resamples for each feature_id.
#' This value will typically be the number of resamples given to `run_resampling()`,
#' but if `run_resampling()` is called with `allow_failures = TRUE` then the number
#' of successful resamples might be less than the number of resamples given.
#'
#' @param qsip_data_object (*qsip_data*) A qsip data object that has been resampled
#' @param as_percentage (*logical*) If TRUE, the x axis will be the percentage of the total number of resamples
#' @param labels (*logical*) If TRUE, the counts will be labeled on the plot
#'
#' @export
#'
#' @returns A ggplot object

plot_successful_resamples <- function(qsip_data_object,
                                      labels = FALSE,
                                      as_percentage = FALSE) {

  # bind variables
  n <- type <- count <- NULL

  is_qsip_resampled(qsip_data_object, error = T)

  if (!is.logical(labels)) {
    stop("<labels> should be TRUE/FALSE", call. = F)
  }

  if (!is.logical(as_percentage)) {
    stop("<as_percentage> should be TRUE/FALSE", call. = F)
  }

  p <- get_resample_counts(qsip_data_object, as_percentage = as_percentage) |>
    tidyr::pivot_longer(cols = c("unlabeled_resamples", "labeled_resamples"),
                        names_to = "type",
                        values_to = "n") |>
    #dplyr::select(feature_id, type, successes = n) |>
    ggplot2::ggplot(ggplot2::aes(x = n, fill = type)) +
    ggplot2::geom_histogram(bins = 20) +
    ggplot2::facet_wrap(~type, ncol = 1) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Successes", y = "feature_id Count") +
    ggplot2::scale_fill_manual(values = c("labeled_resamples" = "#ff0000", "unlabeled_resamples" = "#037bcf"))

  if (isTRUE(labels)) {
    p <- p +
      ggplot2::stat_bin(
        bins = 20, ggplot2::aes(
          y = ggplot2::after_stat(count),
          label = ifelse(ggplot2::after_stat(count) == 0, "", ggplot2::after_stat(count))
        ),
        geom = "text", vjust = -.5
      )
  }

  return(p)
}
