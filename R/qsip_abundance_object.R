#' qSIP abundance table class
#'
#' A class to hold and validate an abundance table
#'
#' @slot data A dataframe or tibble
#' @slot id Column name with unique taxa IDs
#'
#' @export
#'
#' @keywords object

qsip_abundance_object <- S7::new_class(
  "qsip_abundance_object",
  properties = list(
    data = S7::class_data.frame,
    id = S7::class_character,
    taxonomy = S7::class_data.frame
    ),
  validator = function(self) {
      if (any(duplicated(self@data[self@id]))) {
        message(glue::glue("There appear to be duplicate ids in the {self@id} column"))
      }

      qSIP2::abundance_validation(self@data, self@id)

    }
)



#' Add a taxonomy table to qSIP abundance data
#'
#' @param x An object of `qsip_abundance_object` class
#' @param taxa A taxa table
#' @param id The column name for the taxa ids that match the ids in the abundance table
#'
#' @export
#'
#' @keywords taxonomy

add_taxonomy <- S7::new_generic("add_taxonomy", "x")

S7::method(add_taxonomy, qsip_abundance_object) <- function(x, taxa, id) {

  x_ids = x@data |> dplyr::pull(x@id)
  taxa_ids = taxa |> dplyr::pull(id)

  if (length(setdiff(x_ids, taxa_ids)) > 0) {

    stop("some ids found in the abundance object are not found in the taxa table")
  } else if (length(setdiff(taxa_ids, x_ids)) > 0) {
    setdiff(taxa_ids, x_ids)
    stop("some ids found in the taxa table are not found in the abundance object")
  } else {

    # rename taxa id column to the object level ID
    x@taxonomy = taxa |>
      dplyr::rename(!!x@id := id)

    x
  }
}
