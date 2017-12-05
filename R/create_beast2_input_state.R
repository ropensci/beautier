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
  text <- c(text, "<state id=\"state\" storeEvery=\"5000\">")

  text <- c(text, create_beast2_input_state_tree(
    tree_priors = tree_priors,
    initial_phylogenies = initial_phylogenies)
  )

  for (site_model in site_models) {
    new_text <- site_model_to_xml_state(site_model) # nolint internal function
    if (!is.null(new_text)) text <- c(text, new_text)
  }

  for (site_model in site_models) {
    new_text <- indent(
      gamma_site_model_to_xml_state( # nolint internal function
        beautier::get_gamma_site_model(site_model), site_model$id),
      n_spaces = 4
    )
    if (!is.null(new_text)) text <- c(text, new_text)
  }

  clock_models_xml <- clock_models_to_xml_state(clock_models)
  if (length(clock_models_xml) != 0) {
    text <- c(
      text,
      indent(
        clock_models_xml,
        n_spaces = 4
      )
    )
  }

  for (tree_prior in tree_priors) {
    text <- c(
      text,
      beautier::indent(
        tree_prior_to_xml_state(tree_prior),
        n_spaces = 4
      )
    )
  }

  njcsm <- beautier::find_non_jc69_site_model(site_models)
  if (!is.null(njcsm)) {
    text <- c(
      text,
      beautier::indent(
        paste0(
          "<parameter ",
          "id=\"freqParameter.s:", njcsm$id, "\" dimension=\"4\" ",
          "lower=\"0.0\" ",
          "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
        ),
        n_spaces = 4
      )
    )
  }

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
  # Except that the first tree does not have a clockRate

  text <- NULL

  n <- length(tree_priors)
  for (i in seq(1, n)) {
    initial_phylo <- initial_phylogenies[[i]]
    tree_prior <- tree_priors[[i]]
    id <- tree_prior$id

    text <- c(
      text,
      beautier::indent(
        phylo_to_xml_state(id = id, phylo = initial_phylo),
        n_spaces = 4
      )
    )
    # Each tree, except the first, have a clockRate
    # TODO: remove the 1 == 2 statement
    if (i > 1 && 1 == 2) {
      text <- c(text, paste0("    <parameter ",
        "id=\"clockRate.c:", id, "\" ",
        "name=\"stateNode\">1.0</parameter>"))
    }
  } # next i
  text <- c(
    text,
    beautier::indent(
      paste0("<parameter ", "id=\"birthRate.t:", tree_priors[[n]]$id, "\" ",
        "name=\"stateNode\">1.0</parameter>"),
      n_spaces = 4
    )
  )
  text
}
