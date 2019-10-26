#' Creates the tree priors' XML for the tracelog section
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_tracelog_xml}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richèl J.C. Bilderbeek
#' @noRd
tree_priors_to_xml_tracelog <- function(
  tree_priors
) {
  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(text,
      tree_prior_to_xml_tracelog(tree_prior)
    )
  }
  text
}
