#' Creates the tree priors part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param tree_prior a tree prior,
#'   as created by \code{\link{create_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
tree_prior_to_xml_tracelog <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  id <- tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_yule_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"YuleModel.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"birthRate.t:", id, "\"/>"))
  } else if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"BirthDeath.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"BDBirthRate.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"BDDeathRate.t:", id, "\"/>"))
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"popSize.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"CoalescentConstant.t:", id, "\"/>"))
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"BayesianSkyline.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"bPopSizes.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"bGroupSizes.t:", id, "\"/>"))
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"CoalescentExponential.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"ePopSize.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"growthRate.t:", id, "\"/>"))
  }
  text
}
