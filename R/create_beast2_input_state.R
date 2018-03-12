#' Creates the state section of a BEAST2 XML parameter file
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @inheritParams default_params_doc
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state <- function(
  site_models,
  clock_models,
  tree_priors,
  initial_phylogenies,
  mrca_priors = NA
) {
  testit::assert(length(tree_priors) == length(initial_phylogenies))
  testit::assert(are_initial_phylogenies(initial_phylogenies))
  testit::assert(are_tree_priors(tree_priors))

  text <- NULL
  for (i in seq_along(tree_priors)) {
    initial_phylo <- initial_phylogenies[[i]]
    tree_prior <- tree_priors[[i]]
    id <- tree_prior$id
    text <- c(text, phylo_to_xml_state(id = id, phylo = initial_phylo)) # nolint internal function
  }

  text <- c(text, site_models_to_xml_state(site_models)) # nolint internal function
  text <- c(text, clock_models_to_xml_state(clock_models)) # nolint internal function
  text <- c(text, tree_priors_to_xml_state(tree_priors)) # nolint internal function
  text <- c(
    text,
    mrca_priors_to_xml_state( # nolint internal function
      mrca_priors,
      has_non_strict_clock_model = get_has_non_strict_clock_model(clock_models) # nolint internal function
    )
  )

  text <- indent(text, n_spaces = 4) # nolint internal function
  text <- c("<state id=\"state\" storeEvery=\"5000\">", text)
  text <- c(text, "</state>")
  text
}
