#' Creates the tree models' XML for the tracelog section
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @note use site_models just because it contains all IDs
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_beast2_input_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richèl J.C. Bilderbeek
#' @noRd
tree_models_to_xml_tracelog <- function(
  site_models
) {
  text <- NULL
  for (site_model in site_models) {
    id <- site_model$id
    text <- c(text, paste0("<log idref=\"treeLikelihood.", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log id=\"TreeHeight.t:", id, "\" ",
      "spec=\"beast.evolution.tree.TreeHeightLogger\" ",
      "tree=\"@Tree.t:", id, "\"/>") # nolint this is no absolute path
    )
  }
  text
}
