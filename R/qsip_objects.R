#' qSIP source data class
#'
#' @description
#' The `qsip_source_data` object holds validated source material metadata.
#'
#' @details
#' `qsip_source_data()` is not a typical function, but rather a class constructor that
#' instantiates a new `qsip_source_data` object. The constructor takes a `data.frame` as
#' input and returns a validated `qsip_source_data` object.
#'
#' In qSIP and MISIP, "source material" is your original biological specimen that DNA
#' was extracted from. This could be a soil sample, a plant, a mouse, etc. This is the
#' pre-fractionated metadata, and post-fractionation metadata goes in the `qsip_sample_data`
#' object.
#'
#' Several validation checks are run on the input data:
#' * The `data` argument must be a `data.frame`, including a tibble
#' * The `isotope`, `isotopolog`, and `source_mat_id` arguments must be column names in
#'   the `data.frame`
#' * The `source_mat_id` column must be unique
#' * The `isotope` column must contain valid isotope names. "Valid" means they must be
#'   one of the types that the `qSIP2` package has equations for, namely 12C/13C, 14N/15N and 16O/18O.
#'   Some non-isotope names are also valid, including "bulk", "unfractionated" and "T0".
#'
#' Internally, `qsip_source_data` renames the metadata columns to be standardized
#' to MISIP terminology. A `data.frame` with the standardized names can be extracted
#' back out of the object using the `get_dataframe()` method, and the optional `original_headers`
#' argument can be set to `TRUE` to return the original column names.
#'
#' One column of metadata that is required although not used by `qSIP2` is the
#' `isotopolog` column. This column is required to capture complete metadata that
#' is compliant with the MISIP standards. However, when running experiments with multiple
#' isotopologs this column can be used to generate correct comparison groups using
#' the `show_comparison_groups()` function.
#'
#' @param data (*dataframe*) Source metadata
#' @param isotope (*string*) Isotope name
#' @param isotopolog (*string*)  Isotopolog data
#' @param source_mat_id (*string*) The unique ID for the biological subject or source
#' @param timepoint (*string*) Timepoint data
#' @param total_abundance (*string*) Total abundance data
#' @param volume (*string*) Volume of the abundance data. Defaults to 1, but can be a ul volume if abundance data is given as a concentration
#'
#' @family "qSIP Objects"
#'
#' @return A validated `qsip_source_data` object
#'
#' @export

qsip_source_data <- S7::new_class(
  "qsip_source_data",
  package = "qSIP2",
  properties = list(
    data = S7::class_data.frame,
    isotope = S7::class_character,
    isotopolog = S7::class_character,
    source_mat_id = S7::class_character,
    timepoint = S7::class_character,
    total_abundance = S7::class_character,
    volume = S7::class_character
  ),
  constructor = function(data,
                         isotope = "isotope",
                         isotopolog = "isotopolog",
                         source_mat_id = "source_mat_id",
                         timepoint = "NULL",
                         total_abundance = "NULL",
                         volume = "NULL") {

    stopifnot("data should be class <data.frame>" = "data.frame" %in% class(data))

    # verify column names exist
    if (!isotope %in% colnames(data)) {
      stop(glue::glue("isotope column '{isotope}' is not found"), call. = FALSE)
    } else if (!isotopolog %in% colnames(data)) {
      stop(glue::glue("isotopolog column '{isotopolog}' is not found"), call. = FALSE)
    } else if (!source_mat_id %in% colnames(data)) {
      stop(glue::glue("source_mat_id column '{source_mat_id}' is not found"), call. = FALSE)
    } else if (timepoint != "NULL" & !timepoint %in% colnames(data)) {
      stop(glue::glue("timepoint column '{timepoint}' is not found"), call. = FALSE)
    } else if (total_abundance != "NULL" & !total_abundance %in% colnames(data)) {
      stop(glue::glue("total_abundance column '{total_abundance}' is not found"), call. = FALSE)
    } else if (volume != "NULL" & !volume %in% colnames(data)) {
      stop(glue::glue("volume column '{volume}' is not found"), call. = FALSE)
    }



    # rename columns to standardized names
    validate_standard_names(data, source_mat_id, "source")

    ## then rename if all good. doing it like this unfortunately changes the order of the columns though
    data <- data |>
      dplyr::select(
        isotope = dplyr::all_of(isotope),
        isotopolog = dplyr::all_of(isotopolog),
        source_mat_id = dplyr::all_of(source_mat_id),
        dplyr::everything()
      ) |>
      dplyr::ungroup()

    # timepoint
    if (timepoint != "NULL") {
      data <- data |>
        dplyr::select(
          timepoint = dplyr::all_of(timepoint),
          dplyr::everything()
        ) |>
        dplyr::ungroup()

      # verify that timepoint in data is a numeric column
      if (!is.numeric(data$timepoint)) {
        stop(glue::glue("timepoint column '{timepoint}' must be numeric"), call. = FALSE)
      }
    }


    if (total_abundance != "NULL") {

      # stop if volume column is null
      if (volume == "NULL") {
        data = data |>
          dplyr::mutate(volume = 1)
        warning("No <volume> column provided so it is assumed the <total_abundance> column is not a concentration and will not be scaled.", call. = F)
      } else {
        # verify that volume in data is a numeric column
        if (!is.numeric(data$volume)) {
          stop(glue::glue("volume column '{volume}' must be numeric"), call. = FALSE)
        }
        message("Scaling the <total_abundance> values according to the <volume> column.")

      }

      data <- data |>
        dplyr::select(
          total_abundance = dplyr::all_of(total_abundance),
          dplyr::everything()
        ) |>
        dplyr::mutate(total_abundance = total_abundance * volume) |>
        dplyr::ungroup()

      # verify that total_abundance in data is a numeric column

      if (!is.numeric(data$total_abundance)) {
        stop(glue::glue("abundance column '{total_abundance}' must be numeric"), call. = FALSE)
      }
    }



    S7::new_object(S7::S7_object(),
      data = data,
      isotope = isotope,
      isotopolog = isotopolog,
      source_mat_id = source_mat_id,
      timepoint = timepoint,
      total_abundance = total_abundance,
      volume = volume
    )
  },
  validator = function(self) {
    if (any(duplicated(self@data["source_mat_id"]))) {
      stop("some source_mat_ids are duplicated", call. = FALSE)
    }

    qSIP2::validate_isotopes(self@data |> dplyr::pull(isotope))
  }
)






#' qSIP feature table class
#'
#' @description
#' The `qsip_feature_data` object holds validated feature metadata.
#'
#' @details
#' `qsip_feature_data()` is not a typical function, but rather a class constructor that
#' instantiates a new `qsip_feature_data` object. The constructor takes a `data.frame` as
#' input and returns a validated `qsip_feature_data` object.
#'
#' The `qsip_feature_data` object is used to hold feature metadata, such as amplicon
#' sequence variants (ASVs), operational taxonomic units (OTUs), metagenome-assembled
#' genomes (MAGs), etc.
#'
#' The `data` argument takes a `data.frame` that has the feature IDs as
#' a column designated with the `feature_id` argument. Each row corresponds to a unique
#' feature (amplicon, MAG, etc) and each subsequent row corresponds to a unique sample.
#'
#' The `type` argument is used to designate the type of data in the `data` argument. It
#' should most likely be *counts* for amplicon data, and *coverage* for metagenome data
#' (including normalizations like TPM). If the data is relative abundances, the `type`
#' argument should be set to *relative*. Overall, the choice won't much affect the
#' results from the qSIP analysis, but choosing an accurate type will help with the
#' validation checks.
#'
#' Internally, `qsip_feature_data` renames the metadata columns to be standardized
#' to MISIP terminology. A `data.frame` with the standardized names can be extracted
#' back out of the object using the `get_dataframe()` method, and the optional `original_headers`
#' argument can be set to `TRUE` to return the original column names.
#'
#' There are several validation checks run on the data on the `data.frame`:
#' * The `data` argument must contain a `data.frame`, including a tibble
#' * The `feature_id` argument must be a column name in the `data.frame`
#' * The `feature_id` column must contain unique values per row
#' * The `type` argument must be one of *counts*, *coverage* or *relative*
#'     * The `type` argument is *counts* by default, and in this case the values in the
#'  `data` argument must be integers
#'     * If `type` is set to *relative* the values in the `data` argument must be numeric
#'       and the values must sum to 1 for each row
#'     * If `type` is set to *coverage*, the values in the `data` argument must be
#'       numeric
#'     * If the `type` is set to *normalized* then the values are assumed to be pre-normalized and additional
#'       transformations will not be done.
#' * All values in the `data` argument must be non-negative
#'
#' @param data (*dataframe*) ASV/OTU table or equivalent
#' @param feature_id (*string*) Column name with unique taxa IDs
#' @param type (*string, default: counts*) The type of numerical data, either *counts*, *coverage*, *normalized* or *relative*
#'
#' @export
#'
#' @family "qSIP Objects"
#'
#' @returns A validated object of the `qsip_feature_data` type

qsip_feature_data <- S7::new_class(
  "qsip_feature_data",
  package = "qSIP2",
  properties = list(
    data = S7::class_data.frame,
    feature_id = S7::class_character,
    taxonomy = S7::class_data.frame,
    type = S7::class_character
  ),
  constructor = function(data,
                         feature_id = "feature_id",
                         type = "counts") {
    if (!"data.frame" %in% class(data)) {
      stop(glue::glue("data must be class <dataframe>, not {class(data)[1]}"), call. = FALSE)
    }

    if (!feature_id %in% colnames(data)) {
      stop(glue::glue("{feature_id} not found in dataframe"), call. = FALSE)
    }

    # rename columns to standardized names
    validate_standard_names(data, feature_id, "feature")

    ## then rename if all good
    data <- data |>
      dplyr::select(
        feature_id = dplyr::all_of(feature_id),
        dplyr::everything()
      ) |>
      dplyr::ungroup()

    S7::new_object(S7::S7_object(),
      data = data,
      feature_id = feature_id,
      taxonomy = data.frame(),
      type = type
    )
  },
  validator = function(self) {
    if (any(duplicated(self@data["feature_id"]))) {
      stop(glue::glue("There appear to be duplicate ids in the {self@feature_id} column"), call. = FALSE)
    }

    if (!self@type %in% c("counts", "coverage", "normalized", "relative")) {
      stop(glue::glue("feature data type should be 'counts', 'coverage', 'normalized' or 'relative', not '{self@type}'"), call. = FALSE)
    }

    qSIP2::validate_abundances(self@data, "feature_id", type = self@type)
  }
)



#' qSIP sample data class
#'
#' @description
#' The `qsip_sample_data` object holds validated sample metadata.
#'
#' @details
#' `qsip_sample_data()` is not a typical function, but rather a class constructor that
#' instantiates a new `qsip_sample_data` object. The constructor takes a `data.frame` as
#' input and returns a validated `qsip_sample_data` object.
#'
#' In qSIP and MISIP, a "sample" is the post-fractionated material with metadata
#' pertaining to the fractionation process. Sample metadata contains information
#' about the sample and fractionation, such as the sample ID, the source material
#' ID, the gradient position, the density, the amount recovered (e.g. DNA concentration
#' or 16S copies), and the relative abundance of the fraction compared to the total.
#'
#' Ideally, `gradient_pos_amt` should be reported as a mass value of DNA rather than
#' a concentration. However, if the concentration is reported, the `fraction_volume`
#' argument can be used to convert the `gradient_pos_amt` concentration to a mass value.
#' For example, if the `gradient_pos_amt` is reported as ng/ul, and the `fraction_volume`
#' is reported as 100 ul, then the `gradient_pos_amt` will be converted to ng.
#'
#' Internally, `qsip_sample_data` renames the metadata columns to be standardized
#' to MISIP terminology. A `data.frame` with the standardized names can be extracted
#' back out of the object using the `get_dataframe()` method, and the optional `original_headers`
#' argument can be set to `TRUE` to return the original column names.
#'
#' There are several validation checks done on the `data.frame`:
#' * The `data` argument must contain a `data.frame`, including a tibble
#' * The `sample_id` column must contain unique values per row
#' * The `gradient_position` must container positive integers, or `-1` is allowed to
#'   designate the sample as "bulk" or unfractionated
#'
#' @param data (*dataframe*) Metadata for samples/fractions
#' @param sample_id (*string*) The unique sample ID
#' @param source_mat_id (*string*) The unique ID for the biological subject or replicate
#' @param gradient_position (*string*) Column name with the fraction position
#' @param gradient_pos_density (*string*) Column name with the gradient density
#' @param gradient_pos_amt (*string*) Column name with a total amount per fraction, either
#' qPCR copies or DNA
#' @param gradient_pos_rel_amt (*string*) Column name with the relative fraction abundance
#'  compared to the total
#'
#' @export
#' @family "qSIP Objects"
#'
#' @returns A validated object of the `qsip_sample_data` type

qsip_sample_data <- S7::new_class(
  "qsip_sample_data",
  package = "qSIP2",
  properties = list(
    data = S7::class_data.frame,
    sample_id = S7::class_character,
    source_mat_id = S7::class_character,
    gradient_position = S7::class_character,
    gradient_pos_density = S7::class_character,
    gradient_pos_amt = S7::class_character,
    gradient_pos_rel_amt = S7::new_property(S7::class_character, default = "")
  ),
  constructor = function(data,
                         sample_id = "sample_id",
                         source_mat_id = "source_mat_id",
                         gradient_position = "gradient_position",
                         gradient_pos_density = "gradient_pos_density",
                         gradient_pos_amt = "gradient_pos_amt",
                         gradient_pos_rel_amt = "") {
    # make sure data is correct
    stopifnot("data should be class <data.frame>" = "data.frame" %in% class(data))

    # automagically make gradient_pos_rel_amt from gradient_pos_amt, if not specified
    if (gradient_pos_rel_amt == "") {
      message(glue::glue("<gradient_pos_rel_amt> not specified. Calculating using {gradient_pos_amt} column"))
      data = data |>
        add_gradient_pos_rel_amt(source_mat_id = source_mat_id, amt = gradient_pos_amt)
      gradient_pos_rel_amt = "gradient_pos_rel_amt"
    }




    # make sure columns are found
    stopifnot("sample_id column not found" = sample_id %in% colnames(data))
    stopifnot("source_mat_id column not found" = source_mat_id %in% colnames(data))
    stopifnot("gradient_position column not found" = gradient_position %in% colnames(data))
    stopifnot("gradient_pos_density column not found" = gradient_pos_density %in% colnames(data))
    stopifnot("gradient_pos_amt column not found" = gradient_pos_amt %in% colnames(data))
    stopifnot("gradient_pos_rel_amt column not found" = gradient_pos_rel_amt %in% colnames(data))

    # rename columns to standardized names
    validate_standard_names(data, sample_id, "sample")
    data <- data |>
      dplyr::select(
        sample_id = dplyr::all_of(sample_id),
        source_mat_id = dplyr::all_of(source_mat_id),
        gradient_position = dplyr::all_of(gradient_position),
        gradient_pos_density = dplyr::all_of(gradient_pos_density),
        gradient_pos_amt = dplyr::all_of(gradient_pos_amt),
        gradient_pos_rel_amt = dplyr::all_of(gradient_pos_rel_amt),
        dplyr::everything()
      ) |>
      dplyr::ungroup()

    # sample_id column in data should not contain duplicates
    if (any(duplicated(data["sample_id"]))) {
      stop("Some sample_ids are duplicated", call. = FALSE)
    }


    S7::new_object(S7::S7_object(),
      data = data,
      sample_id = sample_id,
      source_mat_id = source_mat_id,
      gradient_position = gradient_position,
      gradient_pos_density = gradient_pos_density,
      gradient_pos_amt = gradient_pos_amt,
      gradient_pos_rel_amt = gradient_pos_rel_amt
    )
  },
  validator = function(self) {
    qSIP2::validate_gradient_pos_density(self@data |> dplyr::select(gradient_position, gradient_pos_density))
    qSIP2::validate_gradient_position(self@data |> dplyr::pull(gradient_position))
  }
)


#' qSIP master data class
#'
#' @description
#' The `qsip_data` object holds validated qSIP source, sample and feature metadata,
#' and has slots to store all of the subsequence `qSIP2` analysis.
#'
#' @details
#' `qsip_data()` is not a typical function, but rather a class constructor that
#' instantiates a new `qsip_data` object. The constructor takes a `qsip_source_data`,
#' `qsip_sample_data` and `qsip_feature_data` as input and returns a validated `qsip_data`
#' object.
#'
#' This `qsip_data` object holds the source, sample and feature data. It also creates empty
#' slots to hold the filtering results, the resampling and the EAF values from their
#' associated functions. For this reason, the `qsip_data` object is intended to be
#' progressively overwritten with new analysis results, but new objects can be created
#' at any point in the analysis, if necessary. For example, a study with multiple comparison
#' groups might be combined into one large `qSIP_data` object, and then split into separate
#' objects at the `run_feature_filtering()` step.
#'
#' Internally, creating the original qSIP objects renames the metadata columns to be standardized
#' to MISIP terminology. A `data.frame` with the standardized names can be extracted
#' back out of the `qSIP_data` using the `get_dataframe()` method and a required `type` argument
#' of "source", "sample" or "feature". The optional `original_headers`
#' argument can be set to `TRUE` to return the original column names.
#'
#' @param source_data (*qsip_source_data*) A qSIP source data object
#' @param sample_data (*qsip_sample_data*) A qSIP sample data object
#' @param feature_data (*qsip_feature_data*) A qSIP feature data object
#'
#' @export
#'
#' @family "qSIP Objects"
#'
#' @return A validated `qsip_data` object

qsip_data <- S7::new_class(
  "qsip_data",
  package = "qSIP2",
  properties = list(
    source_data = S7::class_any,
    sample_data = S7::class_any,
    feature_data = S7::class_any,
    shared = S7::class_list,
    tube_rel_abundance = S7::class_data.frame,
    wads = S7::class_data.frame,
    source_wads = S7::class_data.frame,
    fraction_counts = S7::class_data.frame,
    filtered_feature_data = S7::class_data.frame,
    filtered_wad_data = S7::class_data.frame,
    filter_results = S7::class_list,
    resamples = S7::class_list,
    EAF = S7::class_data.frame,
    growth = S7::class_list
  ),
  constructor = function(source_data,
                         sample_data,
                         feature_data) {

    # make sure data is correct
    stopifnot("source_data should be of class <qsip_source_data>" = inherits(source_data, qsip_source_data))
    stopifnot("sample_data should be of class <qsip_sample_data>" = inherits(sample_data, qsip_sample_data))
    stopifnot("feature_data should be of class <qsip_feature_data>" = inherits(feature_data, qsip_feature_data))

    # calculate tube level relative abundances
    tube_rel_abundance <- calculate_tube_rel_abundance(
      source_data,
      sample_data,
      feature_data
    )

    wad_data <- calculate_wads(tube_rel_abundance)
    source_wad <- calculate_source_wads(sample_data)
    shared <- find_shared_ids(source_data, sample_data, feature_data)

    S7::new_object(S7::S7_object(),
      source_data = source_data,
      sample_data = sample_data,
      feature_data = feature_data,
      shared = shared,
      tube_rel_abundance = tube_rel_abundance,
      wads = wad_data$wads,
      source_wads = source_wad,
      fraction_counts = wad_data$fraction_counts,
      filtered_feature_data = data.frame(),
      filtered_wad_data = data.frame(),
      filter_results = list(),
      resamples = list(),
      EAF = data.frame(),
      growth = list()
    )
  },
  validator = function(self) {
    # # make sure all are valid objects
    # S7::validate(self@source_data)
    # S7::validate(self@sample_data)
    # S7::validate(self@feature_data)
  }
)


# methods
get_dataframe <- S7::new_generic("get_dataframe", "x")

## source data
S7::method(get_dataframe, qsip_source_data) <- function(x, original_headers = FALSE) {

  # if is not boolean
  if (!is.logical(original_headers)) {
    stop(glue::glue("original_headers should be TRUE/FALSE, not {class(original_headers)[1]}"))
  }

  if (isTRUE(original_headers)) {
    x@data |>
      dplyr::rename(
        !!(x@isotope) := isotope,
        !!(x@isotopolog) := isotopolog,
        !!(x@source_mat_id) := source_mat_id
      )
  } else {
    x@data
  }
}

## sample data
S7::method(get_dataframe, qsip_sample_data) <- function(x, original_headers = FALSE) {
  # if is not boolean
  if (!is.logical(original_headers)) {
    stop(glue::glue("original_headers should be TRUE/FALSE, not {class(original_headers)[1]}"))
  }

  if (isTRUE(original_headers)) {
    x@data |>
      dplyr::rename(
        !!(x@sample_id) := sample_id,
        !!(x@source_mat_id) := source_mat_id,
        !!(x@gradient_position) := gradient_position,
        !!(x@gradient_pos_density) := gradient_pos_density,
        !!(x@gradient_pos_amt) := gradient_pos_amt,
        !!(x@gradient_pos_rel_amt) := gradient_pos_rel_amt
      )
  } else {
    x@data
  }
}


#' Return the original dataframe from a qsip_feature_data object
#'
#' @param x (*qsip_feature_data*) A qSIP feature data object
#' @param original_headers (*logical*) Return the original column names
#' @param type (*source*, *sample* or *feature*) When giving a qSIP object the type determines which dataframe to retrieve
#'
#' @name get_dataframe
#' @export


S7::method(get_dataframe, qsip_feature_data) <- function(x, original_headers = FALSE) {
  # if is not boolean
  if (!is.logical(original_headers)) {
    stop(glue::glue("original_headers should be TRUE/FALSE, not {class(original_headers)[1]}"))
  }

  if (isTRUE(original_headers)) {
    x@data |>
      dplyr::rename(!!(x@feature_id) := feature_id)
  } else {
    x@data
  }
}




## qsip data
S7::method(get_dataframe, qsip_data) <- function(x, type, original_headers = FALSE) {
  if (type == "source") {
    d <- x@source_data
  } else if (type == "sample") {
    d <- x@sample_data
  } else if (type == "feature") {
    d <- x@feature_data
  } else {
    stop(glue::glue("<type> should be 'source', 'sample' or 'feature', not {type}"), call. = FALSE)
  }
  # print(d)
  get_dataframe(d, original_headers = original_headers)
}




# extending print methods

S7::method(print, qsip_source_data) <- function(x, ...) {
  sd = S7::prop(x, "data")
  print(glue::glue_col("<qsip_source_data>
                       source_material_id count: {green {length(unique(sd$source_mat_id))}}"))
}

S7::method(print, qsip_sample_data) <- function(x, ...) {
  sd = x@data
  print(glue::glue_col("<qsip_sample_data>
                       source_material_id count: {green {length(unique(sd$source_mat_id))}}
                       sample_id count: {green {length(unique(sd$sample_id))}}"))
}

S7::method(print, qsip_feature_data) <- function(x, ...) {
  print(glue::glue_col("<qsip_feature_data>
                       feature_id count: {green {dim(x@data)[1]}}
                       sample_id count: {green {dim(x@data)[2] - 1}}
                       data type: {green {x@type}}"))
}

S7::method(print, qsip_data) <- function(x, ...) {

  print(glue::glue_col("<qsip_data>
                       group: {green {ifelse(is.null(x@filter_results$group), 'none', x@filter_results$group)}}
                       feature_id count: {green {length(get_feature_ids(x, filtered = is_qsip_filtered(x)))} of {dim(x@feature_data@data)[1]}}
                       sample_id count: {green {length(unique(x@sample_data@data$sample_id))}}
                       filtered: {green {is_qsip_filtered(x)}}
                       resampled: {green {is_qsip_resampled(x)}}
                       EAF: {green {is_qsip_EAF(x)}}
                       growth: {green {is_qsip_growth(x)}}"))
}
