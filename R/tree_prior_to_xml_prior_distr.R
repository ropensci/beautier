#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#'
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
tree_prior_to_xml_prior_distr <- function(
  tree_prior,
  beauti_options
) {
  check_tree_prior(tree_prior)
  check_beauti_options(beauti_options)
  text <- NULL
  if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, bd_tree_prior_to_xml_prior_distr(tree_prior, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, cbs_tree_prior_to_xml_prior_distr(tree_prior, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, ccp_tree_prior_to_xml_prior_distr(tree_prior, beauti_options = beauti_options)) # nolint indeed a long line
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, cep_tree_prior_to_xml_prior_distr(tree_prior, beauti_options = beauti_options)) # nolint indeed a long line
  } else {
    check_true(is_yule_tree_prior(tree_prior))
    text <- c(text, yule_tree_prior_to_xml_prior_distr(tree_prior, beauti_options = beauti_options)) # nolint indeed a long line
  }
  text
}
