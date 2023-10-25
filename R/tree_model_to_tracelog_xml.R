#' Internal function
#'
#' Creates the tree models' XML for the tracelog section.
#' That is, all XML tags that have the word 'tree' in them.
#'
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
tree_model_to_tracelog_xml <- function(
  inference_model
) {
  id <- inference_model$site_model$id
  check_true(is_id(id))
  text <- NULL
  text <- c(text, paste0("<log idref=\"treeLikelihood.", id, "\"/>")) # nolint this is no absolute path
  text <- c(
    text,
    paste0(
      "<log id=\"TreeHeight.t:", id, "\" ",
      "spec=\"beast.evolution.tree.TreeHeightLogger\" ",
      "tree=\"@Tree.t:", id, "\"/>"
    )
  )
  text
}
