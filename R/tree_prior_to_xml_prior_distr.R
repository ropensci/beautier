#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
tree_prior_to_xml_prior_distr <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  text <- NULL
  if (beautier::is_bd_tree_prior(tree_prior)) {
    text <- c(text, bd_tree_prior_to_xml_prior_distr(tree_prior)) # nolint internal function
  } else if (beautier::is_cbs_tree_prior(tree_prior)) {
    text <- c(text, cbs_tree_prior_to_xml_prior_distr(tree_prior)) # nolint internal function
  } else if (beautier::is_ccp_tree_prior(tree_prior)) {
    text <- c(text, ccp_tree_prior_to_xml_prior_distr(tree_prior)) # nolint internal function
  } else if (beautier::is_cep_tree_prior(tree_prior)) {
    text <- c(text, cep_tree_prior_to_xml_prior_distr(tree_prior)) # nolint internal function
  } else {
    testit::assert(beautier::is_yule_tree_prior(tree_prior))
    text <- c(text, yule_tree_prior_to_xml_prior_distr(tree_prior)) # nolint internal function
  }
  text
}
