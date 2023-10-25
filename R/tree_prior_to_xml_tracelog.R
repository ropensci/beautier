#' Creates the tree prior's XML for the tracelog section
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @seealso all tree priors' tracelog section is created
#'   by \code{\link{tree_priors_to_xml_tracelog}}
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
tree_prior_to_xml_tracelog <- function(
  tree_prior
) {
  check_true(is_tree_prior(tree_prior))
  id <- tree_prior$id
  check_true(is_id(id))

  text <- NULL
  if (is_yule_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"YuleModel.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"birthRate.t:", id, "\"/>")) # nolint this is no absolute path
  } else if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"BirthDeath.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"BDBirthRate.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"BDDeathRate.t:", id, "\"/>")) # nolint this is no absolute path
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"popSize.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"CoalescentConstant.t:", id, "\"/>")) # nolint this is no absolute path
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"BayesianSkyline.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"bPopSizes.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"bGroupSizes.t:", id, "\"/>")) # nolint this is no absolute path
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"CoalescentExponential.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"ePopSize.t:", id, "\"/>")) # nolint this is no absolute path
    text <- c(text, paste0("<log idref=\"growthRate.t:", id, "\"/>")) # nolint this is no absolute path
  }
  text
}
