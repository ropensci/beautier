#' Creates the XML of a tree prior,
#'   as used in the \code{state} section
#' @inheritParams default_params_doc
#' @return the tree prior as XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
tree_prior_to_xml_state <- function(
  tree_prior
) {
  testit::assert(is_tree_prior(tree_prior)) # nolint beautier function
  id <- tree_prior$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  if (is_bd_tree_prior(tree_prior)) { # nolint beautier function
    text <- c(text, paste0("<parameter id=\"BDBirthRate.t:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"BDDeathRate.t:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>"))
  } else if (is_ccp_tree_prior(tree_prior)) { # nolint beautier function
    text <- c(text, paste0("<parameter id=\"popSize.t:", id, "\" ",
      "name=\"stateNode\">0.3</parameter>"))
  } else if (is_cbs_tree_prior(tree_prior)) { # nolint beautier function
    text <- c(text, paste0("<parameter id=\"bPopSizes.t:", id, "\" ",
      "dimension=\"5\" lower=\"0.0\" name=\"stateNode\" ",
      "upper=\"380000.0\">380.0</parameter>"))
    text <- c(
      text,
      paste0(
        "<stateNode id=\"bGroupSizes.t:", id, "\" ",
        "spec=\"parameter.IntegerParameter\" ",
        "dimension=\"", tree_prior$group_sizes_dimension, "\">1</stateNode>"
      )
    )
  } else if (is_cep_tree_prior(tree_prior)) { # nolint beautier function
    text <- c(text, paste0("<parameter id=\"ePopSize.t:", id, "\" ",
      "name=\"stateNode\">0.3</parameter>"))
    text <- c(text, paste0("<parameter id=\"growthRate.t:", id, "\" ",
      "name=\"stateNode\">3.0E-4</parameter>"))
  } else {
    testit::assert(is_yule_tree_prior(tree_prior)) # nolint beautier function
      text <- c(text, paste0("<parameter ", "id=\"birthRate.t:", id, "\" ",
        "name=\"stateNode\">1.0</parameter>"))
  }
  text
}
