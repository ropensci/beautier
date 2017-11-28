#' Creates the XML of a tree prior,
#'   as used in the \code{prior} section
#' @param tree_prior a tree prior,
#'   as returned by \code{\link{create_tree_prior}})
#' @return the tree prior as XML text
#' @author Richel J.C. Bilderbeek
tree_prior_to_xml_prior <- function(
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  text <- NULL
  if (is_yule_tree_prior(tree_prior)) {

    text <- c(text,
      yule_tree_prior_to_xml_prior(
        yule_tree_prior = tree_prior
      )
    )

  } else if (is_bd_tree_prior(tree_prior)) {

    text <- c(
      text,
      bd_tree_prior_to_xml_prior(
        bd_tree_prior = tree_prior
      )
    )

  } else if (is_ccp_tree_prior(tree_prior)) {

    text <- c(
      text,
      create_beast2_input_distr_prior_prior_tree_prior_ccp(
        ccp_tree_prior = tree_prior
      )
    )

  } else if (is_cep_tree_prior(tree_prior)) {

    text <- c(
      text,
      create_beast2_input_distr_prior_prior_tree_prior_cep(
        cep_tree_prior = tree_prior
      )
    )
  }

  text
}
