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
    stop("Input must be a qsip object or list of qsip objects", call. = F)
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
