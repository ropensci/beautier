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
  site_models = create_site_model(name = "JC69"),
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

  text <- NULL
  text <- c(text, "    <state id=\"state\" storeEvery=\"5000\">")
  text <- c(text, create_beast2_input_state_tree(
    ids = ids, initial_phylogenies = initial_phylogenies))
  text <- c(text, create_beast2_input_state_tree_priors(
    ids = ids, tree_priors = tree_priors))
  text <- c(text, create_beast2_input_state_site_models_1(
    ids = ids, site_models = site_models))
  text <- c(text, create_beast2_input_state_gamma_site_models_1(
    ids = ids, site_models = site_models))
  text <- c(text, create_beast2_input_state_site_models_2(
    ids = ids, site_models = site_models))
  text <- c(text, create_beast2_input_state_gamma_site_models_2(
    ids = ids, site_models = site_models))


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
    if (!ribir::is_phylogeny(initial_phylogeny)) {
      text <- c(text, paste0("        <tree id=\"Tree.t:",
        id, "\" name=\"stateNode\">"))
      text <- c(text, paste0("            <taxonset id=\"TaxonSet.",
        id, "\" spec=\"TaxonSet\">"))
      text <- c(text, paste0("                <alignment idref=\"",
        id, "\"/>"))
      text <- c(text, "            </taxonset>")
      text <- c(text, "        </tree>")
    } else {
      text <- c(text, paste0("    <stateNode spec=\"beast.util.TreeParser\" ",
          "id=\"Tree.t:", id, "\" IsLabelledNewick=\"true\" ",
          "adjustTipHeights=\"false\" taxa=\"@", id, "\" ",
          "newick=\"", ape::write.tree(initial_phylogeny), "\">"))
      text <- c(text, paste0("    </stateNode>"))
    }
  }
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
  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <parameter id=\"birthRate.t:", ids, "\" ",
      "name=\"stateNode\">1.0</parameter>"))
  } else if (is_bd_tree_prior(tree_priors)) {
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

#' Creates the first site_models part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_site_models_1 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("        <parameter id=\"kappa.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">",
      beautier::get_kappa(site_models), "</parameter>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0("        <parameter id=\"kappa1.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"kappa2.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">2.0</parameter>"))
  }
  text
}

#' Creates the second site_models part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_site_models_2 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (is_gtr_site_model(site_models)) {
    text <- c(text, paste0("        <parameter id=\"rateAC.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateAG.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateAT.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateCG.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
    text <- c(text, paste0("        <parameter id=\"rateGT.s:", ids, "\" ",
      "lower=\"0.0\" name=\"stateNode\">1.0</parameter>"))
  }
  text
}

#' Creates the gamma_site_models part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_gamma_site_models_1 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  gamma_site_models <- beautier::get_gamma_site_model(
    site_models = site_models)
  if (get_gamma_cat_count(gamma_site_models) > 1) {
    gamma_shape <- beautier::get_gamma_shape(gamma_site_models)
    text <- c(text, paste0("        <parameter ",
      "id=\"gammaShape.s:", ids, "\" ",
      "name=\"stateNode\">", gamma_shape, "</parameter>"))
  }
  if (is_jc69_site_model(site_models)) return(text)

  if (get_gamma_cat_count(get_gamma_site_model(site_models)) > 0) {
    text <- c(text, paste0("        <parameter ",
      "id=\"freqParameter.s:", ids, "\" dimension=\"4\" lower=\"0.0\" ",
      "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"))
  }
  text
}

#' Creates the second gamma_site_models part of the state section of a BEAST2
#' XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_state_gamma_site_models_2 <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (is_jc69_site_model(site_models)) return(text)
  gamma_site_models <- beautier::get_gamma_site_model(
    site_models = site_models)
  if (get_gamma_cat_count(gamma_site_models) == 0) {
    text <- c(text, paste0("        <parameter ",
      "id=\"freqParameter.s:", ids, "\" dimension=\"4\" lower=\"0.0\" ",
      "name=\"stateNode\" upper=\"1.0\">0.25</parameter>"))
  }
  text
}
