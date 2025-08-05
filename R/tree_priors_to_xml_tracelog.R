#' Creates the tree priors' XML for the tracelog section
#' @inheritParams default_params_doc
#' @return lines of XML text
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
tree_priors_to_xml_tracelog <- function(
  tree_priors
) {
  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(text,
      beautier::tree_prior_to_xml_tracelog(tree_prior)
    )
  }
  text
}
