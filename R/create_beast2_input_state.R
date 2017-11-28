#' Creates the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state <- function(
  ids,
  site_models,
  clock_models,
  tree_priors,
  initial_phylogenies
) {
  testit::assert(class(initial_phylogenies) == "multiPhylo" ||
      is.na(initial_phylogenies))
  testit::assert(beautier::are_ids(ids))
  testit::assert(length(ids) == length(initial_phylogenies))
  testit::assert(length(ids) >= length(site_models))
  testit::assert(length(ids) >= length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL
  text <- c(text, "<state id=\"state\" storeEvery=\"5000\">")

  text <- c(text, create_beast2_input_state_tree(
    tree_priors = tree_priors,
    initial_phylogenies = initial_phylogenies)
  )
  for (tree_prior in tree_priors) {
    text <- c(text, create_beast2_input_state_tree_prior(
      tree_prior = tree_prior))
  }

  for (site_model in site_models) {

    # There are three parts:
    # 1) rates
    # 2) freq
    # 3) gamma shape
    # Order is determined by site model and Gamma Category Count :-(
    rates <- site_model_to_xml_rates(site_model = site_model) # nolint internal function
    if (!is.null(rates)) {
      rates <- beautier::indent(rates, n_spaces = 4)
    }

    freqparams <- create_beast2_input_state_gamma_site_models_freqparams(site_model = site_model) # nolint
    gamma_shape <- create_beast2_input_state_gamma_site_models_gamma_shape(site_model = site_model) # nolint
    gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model)) # nolint
    prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model)) # nolint
    if (gcc == 0) {
      text <- c(text, rates)
      text <- c(text, freqparams)
    } else if (gcc == 1) {
      if (is_gtr_site_model(site_model)) {
        text <- c(text, freqparams)
        text <- c(text, rates)
      } else {
        text <- c(text, rates)
        text <- c(text, freqparams)
      }
    } else {
      if (is_gtr_site_model(site_model)) {
        if (prop_invariant == get_default_prop_invariant()) {
          text <- c(text, freqparams)
          text <- c(text, rates)
          text <- c(text, gamma_shape)
        } else {
          text <- c(text, gamma_shape)
          text <- c(text, freqparams)
          text <- c(text, rates)
        }
      } else {
        text <- c(text, rates)
        text <- c(text, gamma_shape)
        text <- c(text, freqparams)
      }
    }
  }
  for (clock_model in clock_models) {
    text <- c(text, create_beast2_input_state_clock_model(
      clock_model = clock_model)
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
  # <parameter id="clockRate.c:anthus_nd4" name="stateNode">1.0</parameter>
  # <parameter id="birthRate.t:anthus_nd4" name="stateNode">1.0</parameter>
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
    if (i > 1) {
      text <- c(text, paste0("    <parameter ",
        "id=\"clockRate.c:", id, "\" ",
        "name=\"stateNode\">1.0</parameter>"))
    }
    tree_prior_text <- tree_prior_to_xml_state(tree_prior = tree_prior) # nolint internal function
    if (!is.null(tree_prior_text)) {
      text <- c(text, beautier::indent(tree_prior_text, n_spaces = 4))
    }
  } # next i
  text
}


#' Creates the tree priors part of the state section of a BEAST2
#' XML parameter file
#' @param tree_prior tree prior, as created by \code{\link{create_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state_tree_prior <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  id <- tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, paste0("<parameter id=\"BDBirthRate.t:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>"))
    text <- c(text, paste0("<parameter id=\"BDDeathRate.t:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>"))
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, paste0("<parameter id=\"popSize.t:", id, "\" ",
      "name=\"stateNode\">0.3</parameter>"))
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, paste0("<parameter id=\"bPopSizes.t:", id, "\" ",
      "dimension=\"5\" lower=\"0.0\" name=\"stateNode\" ",
      "upper=\"380000.0\">380.0</parameter>"))
    text <- c(text, paste0("<stateNode id=\"bGroupSizes.t:", id, "\" ",
      "spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>"))
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, paste0("<parameter id=\"ePopSize.t:", id, "\" ",
      "name=\"stateNode\">0.3</parameter>"))
    text <- c(text, paste0("<parameter id=\"growthRate.t:", id, "\" ",
      "name=\"stateNode\">3.0E-4</parameter>"))
  }
  if (!is.null(text)) {
    text <- beautier::indent(text, n_spaces = 4)
  }
  text
}

#' Creates the gamma_site_models part of the state section of a BEAST2
#' XML parameter file
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state_gamma_site_models_gamma_shape <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  text <- c(text, paste0("    <parameter ",
    "id=\"gammaShape.s:", id, "\" ",
    "name=\"stateNode\">",
    beautier::get_gamma_shape(get_gamma_site_model(site_model)),
    "</parameter>")
  )
  text
}

#' Creates the freqParameters gamma_site_models part of the state section of
#' a BEAST2 XML parameter file
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state_gamma_site_models_freqparams <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (!is_jc69_site_model(site_model)) {
    text <- c(text, paste0("    <parameter ",
      "id=\"freqParameter.s:", id, "\" dimension=\"4\" lower=\"0.0\" ",
      "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"))
  }
  text
}

#' Creates the clock models' part of the state section of
#' a BEAST2 XML parameter file
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_state_clock_model <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_rln_clock_model(clock_model)) {
    text <- c(text, paste0("<parameter id=\"ucldStdev.c:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">0.1</parameter>"))
    text <- c(text, paste0("<stateNode ",
      "id=\"rateCategories.c:", id, "\" ",
      "spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>"))
    text <- beautier::indent(text, n_spaces = 4)
  }
  text
}
