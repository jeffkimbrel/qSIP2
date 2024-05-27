#' Validate a multi-qsip list object
#'
#'
#' @export

validate_multi_qsip <- function(qsip_list) {

  # check if qsip_list is a list
  if(!is.list(qsip_list)){
    stop("qsip_list must be a list")
  }

  class_list = lapply(qsip_list, class)

  j = sapply(class_list, function(x) {
    if(TRUE %in% stringr::str_detect(x, paste("qsip_data", collapse = "|"))){
      TRUE
    } else {FALSE}
  })

  all(j)
}
