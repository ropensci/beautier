#' Creates the tree models' XML for the tracelog section
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @note use site_models just because it contains all IDs
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_tracelog_xml}}
#' @examples
#' check_empty_beautier_folder()
#'
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
tree_models_to_xml_tracelog <- function(
  site_models
) {
  stop(
    "'tree_models_to_xml_tracelog' is deprecated, ",
    "use 'tree_model_to_tracelog_xml' instead"
  )

}
