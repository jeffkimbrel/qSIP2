#' qSIP taxa table class
#'
#' A class to hold and validate taxa abundance table
#'
#' @slot data A dataframe or tibble
#' @slot id Column name with unique taxa IDs
#'
#' @export
#'
#' @keywords sample_data

qsip_taxa_object <- S7::new_class(
  "qsip_taxa_object",
  properties = list(
    data = S7::class_data.frame,
    id = S7::new_property(S7::class_character)
    ),
  validator = function(self) {
      if (any(duplicated(self@data[self@id]))) {
        message(glue::glue("There appear to be duplicate ids in the {self@id} column"))
      }
      qSIP2::taxa_abundance_validation(self@data, self@id)

    }
)
