#' Creates the tree priors' XML for the tracelog section
#' @inheritParams default_params_doc
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_beast2_input_tracelog}}
#' @examples
#' # <logger id="tracelog" ...>
#' #'   # Here
#' # </logger>
#' @author Richel J.C. Bilderbeek
tree_priors_to_xml_tracelog <- function(
  tree_priors
) {
  if (length(tree_priors) != length(get_unlinked_tree_priors(tree_priors))) {
    stop("Cannot have linked tree priors")
  }

  text <- NULL
  for (tree_prior in tree_priors) {
    text <- c(text,
      tree_prior_to_xml_tracelog(tree_prior)
    )
  }
  text
}
