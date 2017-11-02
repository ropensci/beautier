#' Creates the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param initial_phylogenies initial phylogenies, can be NAs if random
#'   phylogenies are desired
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state <- function(
  ids,
  site_models = create_jc69_site_models(length(ids)),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule"),
  initial_phylogenies = rep(NA, length(ids))
) {
  if (class(initial_phylogenies) == "phylo") {
    initial_phylogenies <- c(initial_phylogenies)
    testit::assert(class(initial_phylogenies) == "multiPhylo")
  }
  if (length(ids) != length(initial_phylogenies)) {
    stop("Must supply as much IDs as initial_phylogenies")
  }
  if (length(ids) != length(site_models)) {
    stop("Must supply as much IDs as site_model objects")
  }

  text <- NULL
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")
  text <- c(text, create_beast2_input_state_tree(
    ids = ids, tree_priors = tree_priors,
    initial_phylogenies = initial_phylogenies)
  )

  # Birth: always first
  text <- c(text, create_beast2_input_state_tree_priors(
    ids = ids, tree_priors = tree_priors))

  n <- length(ids)
  for (i in seq(1, n)) {
    id <- ids[i]
    site_model <- site_models[[i]]
    # There are three parts:
    # 1) rates
    # 2) freq
    # 3) gamma shape
    # Order is determined by site model and Gamma Category Count :-(
    rates <- beautier::create_beast2_input_state_site_models_rates(id = id, site_model = site_model) # nolint
    freq_parameters <- beautier::create_beast2_input_state_gamma_site_models_freq_parameters(id = id, site_model = site_model) # nolint
    gamma_shape <- beautier::create_beast2_input_state_gamma_site_models_gamma_shape(id = id, site_model = site_model) # nolint
    gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model)) # nolint
    prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model)) # nolint
    if (gcc == 0) {
      text <- c(text, rates)
      text <- c(text, freq_parameters)
    } else if (gcc == 1) {
      if (is_gtr_site_model(site_model)) {
        text <- c(text, freq_parameters)
        text <- c(text, rates)
      } else {
        text <- c(text, rates)
        text <- c(text, freq_parameters)
      }
    } else {
      if (is_gtr_site_model(site_model)) {
        if (prop_invariant == get_default_prop_invariant()) {
          text <- c(text, freq_parameters)
          text <- c(text, rates)
          text <- c(text, gamma_shape)
        } else {
          text <- c(text, gamma_shape)
          text <- c(text, freq_parameters)
          text <- c(text, rates)
        }
      } else {
        text <- c(text, rates)
        text <- c(text, gamma_shape)
        text <- c(text, freq_parameters)
      }
    }
  }

  # Clock models
  if (is_rln_clock_model(clock_models)) {
    text <- c(text, paste0("        <parameter id=\"ucldStdev.c:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">0.1</parameter>"))
    text <- c(text, paste0("        <stateNode ",
      "id=\"rateCategories.c:", ids, "\" ",
      "spec=\"parameter.IntegerParameter\" dimension=\"8\">1</stateNode>"))
  }

  text <- c(text, "    </state>")
  text
}

#' Creates the tree part of the state section of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_id}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_tree <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors = create_tree_prior(name = "yule"),
  initial_phylogenies = rep(NA, length(ids))
) {
  if (length(ids) != length(initial_phylogenies)) {
    stop("Must supply as much IDs as initial_phylogenies")
  }

  text <- NULL

  n <- length(ids)
  for (i in seq(1, n)) {
    initial_phylogeny <- initial_phylogenies[[i]]
    id <- ids[i]
    tree_prior <- tree_priors[i]

    if (!ribir::is_phylogeny(initial_phylogeny)) {
      text <- c(text, paste0("        <tree id=\"Tree.t:",
        id, "\" name=\"stateNode\">"))
      text <- c(text, paste0("            <taxonset id=\"TaxonSet.",
        id, "\" spec=\"TaxonSet\">"))
      text <- c(text, paste0("                <alignment idref=\"",
        id, "\"/>"))
      text <- c(text, "            </taxonset>")
      text <- c(text, "        </tree>")

      testit::assert(length(id) == 1)
      if (n > 1 && i == 2) {
        text <- c(text, paste0("        <parameter ",
          "id=\"clockRate.c:", id, "\" ",
          "name=\"stateNode\">1.0</parameter>"))
      }
    } else {
      text <- c(text, paste0("    <stateNode spec=\"beast.util.TreeParser\" ",
          "id=\"Tree.t:", id, "\" IsLabelledNewick=\"true\" ",
          "adjustTipHeights=\"false\" taxa=\"@", id, "\" ",
          "newick=\"", ape::write.tree(initial_phylogeny), "\">"))
      text <- c(text, paste0("    </stateNode>"))
    }
    if (is_yule_tree_prior(tree_prior)) {
      text <- c(text, paste0("        <parameter ",
        "id=\"birthRate.t:", id, "\" ",
        "name=\"stateNode\">1.0</parameter>"))
    }
  } # next i
  text
}


#' Creates the tree priors part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_tree_priors <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors
) {
  text <- NULL
  if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"BDBirthRate.t:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\" upper=\"10000.0\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"BDDeathRate.t:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.5</parameter>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"popSize.t:", ids, "\" ",
      "name=\"stateNode\">0.3</parameter>"))
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"bPopSizes.t:", ids, "\" ",
      "dimension=\"5\" lower=\"0.0\" name=\"stateNode\" ",
      "upper=\"380000.0\">380.0</parameter>"))
    text <- c(text, paste0("        <stateNode id=\"bGroupSizes.t:", ids, "\" ",
      "spec=\"parameter.IntegerParameter\" dimension=\"5\">1</stateNode>"))
  }
  text
}

#' Creates the reates of the site_models part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_site_models_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("        <parameter id=\"rateAC.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateAG.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateAT.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateCG.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateGT.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
  } else if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("        <parameter id=\"kappa.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">",
      beautier::get_kappa(site_model), "</parameter>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("        <parameter id=\"kappa1.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"kappa2.s:", id, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
  }
  text
}

#' Creates the gamma_site_models part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_gamma_site_models_gamma_shape <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  text <- c(text, paste0("        <parameter ",
    "id=\"gammaShape.s:", id, "\" ",
    "name=\"stateNode\">",
    beautier::get_gamma_shape(get_gamma_site_model(site_model)),
    "</parameter>")
  )
  text
}

#' Creates the freqParameters gamma_site_models part of the state section of
#' a BEAST2 XML parameter file
#' @param id the ID of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_gamma_site_models_freq_parameters <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (!is_jc69_site_model(site_model)) {
    text <- c(text, paste0("        <parameter ",
      "id=\"freqParameter.s:", id, "\" dimension=\"4\" lower=\"0.0\" ",
      "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"))
  }
  text
}
