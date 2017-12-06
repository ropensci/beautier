#' Creates the state section of a BEAST2 XML parameter file
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state <- function(
  site_models,
  clock_models,
  tree_priors,
  initial_phylogenies
) {
  testit::assert(class(initial_phylogenies) == "multiPhylo" ||
      is.na(initial_phylogenies))
  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL
  text <- c(
    text,
    create_beast2_input_state_tree(
      tree_priors = tree_priors,
      initial_phylogenies = initial_phylogenies
    )
  )

  text <- c(text, site_models_to_xml_state(site_models)) # nolint internal function
  text <- c(text, clock_models_to_xml_state(clock_models)) # nolint internal function
  text <- c(text, tree_priors_to_xml_state(tree_priors)) # nolint internal function

  text <- beautier::indent(text, n_spaces = 4)
  text <- c("<state id=\"state\" storeEvery=\"5000\">", text)
  text <- c(text, "</state>")
  beautier::indent(text, n_spaces = 4)
}

#' Creates the tree part of the state section of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state_tree <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_priors,
  initial_phylogenies
) {

  testit::assert(length(tree_priors) == length(initial_phylogenies))
  # Each tree looks like this:
  #
  # <tree id="Tree.t:anthus_nd4" name="stateNode">
  #     <taxonset id="TaxonSet.anthus_nd4" spec="TaxonSet">
  #         <alignment idref="anthus_nd4"/>
  #     </taxonset>
  # </tree>
  #

  text <- NULL

  n <- length(tree_priors)
  for (i in seq(1, n)) {
    initial_phylo <- initial_phylogenies[[i]]
    tree_prior <- tree_priors[[i]]
    id <- tree_prior$id

    text <- c(text, phylo_to_xml_state(id = id, phylo = initial_phylo))
  } # next i
  text
}
