#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_site_model(name = "JC69"),
  clock_models = create_clock_model(name = "strict"),
  tree_priors = create_tree_prior(name = "yule")
) {

  text <- NULL

  text <- c(text, paste0("    <logger id=\"tracelog\" fileName=\"",
    ids, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">"))
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, paste0("        <log idref=\"treeLikelihood.",
    ids, "\"/>"))
  text <- c(text, paste0("        <log id=\"TreeHeight.t:", ids,
    "\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:",
    ids, "\"/>"))

  text <- c(text, beautier::create_beast2_input_loggers_tree_priors(
    ids = ids, tree_priors = tree_priors))

  # Now three things
  rates <- beautier::create_beast2_input_loggers_rates(ids = ids, site_models = site_models) # nolint
  freq_parameters <- beautier::create_beast2_input_loggers_freq_parameter(ids = ids, site_models = site_models) # nolint
  gamma_shape <- beautier::create_beast2_input_loggers_gamma_shape(ids = ids, site_models = site_models) # nolint
  gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_models)) # nolint
  prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_models)) # nolint

  if (is_gtr_site_model(site_models)) {
    if (gcc == 0) {
      text <- c(text, rates)
      text <- c(text, freq_parameters)
    } else if (gcc == 1) {
      text <- c(text, freq_parameters)
      text <- c(text, rates)
    } else {
      # gcc >= 2
      if (prop_invariant == get_default_prop_invariant()) {
        text <- c(text, freq_parameters)
        text <- c(text, rates)
        text <- c(text, gamma_shape)
      } else {
        text <- c(text, gamma_shape)
        text <- c(text, freq_parameters)
        text <- c(text, rates)
      }
    }
  } else {
    text <- c(text, rates)
    text <- c(text, gamma_shape)
    text <- c(text, freq_parameters)
  }



  text <- c(text, beautier::create_beast2_input_loggers_clock_models(
    ids = ids, clock_models = clock_models))

  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, "    <logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, paste0("        <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"))
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, "    </logger>")
  text <- c(text, "")
  text <- c(text, paste0("    <logger id=\"treelog.t:", ids, "\" ",
    "fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">"))

  # Clock models
  if (is_strict_clock_model(clock_models)) {
    text <- c(text, paste0("        <log ",
      "id=\"TreeWithMetaDataLogger.t:", ids, "\" ",
      "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
      "tree=\"@Tree.t:", ids, "\"/>"))
  } else if (is_rln_clock_model(clock_models)) {
    text <- c(text, paste0("        <log ",
      "id=\"TreeWithMetaDataLogger.t:", ids, "\" ",
      "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
      "branchratemodel=\"@RelaxedClock.c:", ids, "\" ",
      "tree=\"@Tree.t:", ids, "\"/>"))
  }

  text <- c(text, "    </logger>")
  text
}

#' Creates the tree priors part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_tree_priors <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors
) {
  text <- NULL
  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"YuleModel.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"birthRate.t:",
      ids, "\"/>"))
  } else if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"BirthDeath.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDBirthRate.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDDeathRate.t:",
      ids, "\"/>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"popSize.t:",
      ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"CoalescentConstant.t:",
      ids, "\"/>"))
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log ",
      "idref=\"BayesianSkyline.t:", ids, "\"/>"))
    text <- c(text, paste0("        <log ",
      "idref=\"bPopSizes.t:", ids, "\"/>"))
    text <- c(text, paste0("        <log ",
      "idref=\"bGroupSizes.t:", ids, "\"/>"))
  }
  text
}

#' Creates the first site models part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL

  if (is_hky_site_model(site_models)) {
    text <- c(text, paste0("        <log idref=\"kappa.s:", ids, "\"/>"))
  } else if (is_tn93_site_model(site_models)) {
    text <- c(text, paste0("        <log idref=\"kappa1.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"kappa2.s:", ids, "\"/>"))
  } else if (is_gtr_site_model(site_models)) {
    text <- c(text, paste0("        <log idref=\"rateAC.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateAG.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateAT.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateCG.s:", ids, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateGT.s:", ids, "\"/>"))
  }

  text
}

#' Creates the freqParameter part of the log sections
#'   of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_freq_parameter <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (!is_jc69_site_model(site_models)) {
    text <- c(text, paste0("        <log ",
      "idref=\"freqParameter.s:", ids, "\"/>"))
  }
  text
}

#' Creates the gammaShape part of the log sections
#'   of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_gamma_shape <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models
) {
  text <- NULL
  if (get_gamma_cat_count(get_gamma_site_model(site_models)) > 1) {
    text <- c(text, paste0("        <log idref=\"gammaShape.s:", ids, "\"/>"))
  }
  text
}

#' Creates the clock models part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_clock_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  clock_models
) {
  text <- NULL
  if (is_rln_clock_model(clock_models)) {
    text <- c(text, paste0("        <log idref=\"ucldStdev.c:", ids, "\"/>"))
    text <- c(text, paste0("        <log id=\"rate.c:", ids, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", ids, "\" ",
      "tree=\"@Tree.t:", ids, "\"/>"))
  }
  text
}
