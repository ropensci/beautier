#' Creates the two logger sections of a BEAST2 XML parameter file
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_jc69_site_models(n = length(ids)),
  clock_models = create_strict_clock_models(n = length(ids)),
  tree_priors = create_yule_tree_priors(n = length(ids))
) {
  if (is_site_model(site_models)) {
    site_models <- list(site_models)
  }
  if (is_clock_model(clock_models)) {
    clock_models <- list(clock_models)
  }
  if (is_tree_prior(tree_priors)) {
    tree_priors <- list(tree_priors)
  }
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(are_site_models(site_models))
  testit::assert(are_clock_models(clock_models))
  testit::assert(are_tree_priors(tree_priors))

  text <- NULL
  text <- c(text, beautier::create_beast2_input_tracelog(
    ids = ids,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors))

  text <- c(text, beautier::create_beast2_input_screenlog())

  text <- c(text, beautier::create_beast2_input_treelogs(
    ids = ids,
    clock_models = clock_models))

  text
}


#' Creates the tracelog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_tracelog <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_jc69_site_models(n = length(ids)),
  clock_models = create_strict_clock_models(n = length(ids)),
  tree_priors = create_yule_tree_priors(n = length(ids))
) {
  if (length(ids) != length(site_models)) {
    stop("Must supply as much IDs as site models")
  }
  if (length(ids) != length(clock_models)) {
    stop("Must supply as much IDs as clock models")
  }
  if (length(ids) != length(tree_priors)) {
    stop("Must supply as much IDs as tree priors")
  }
  text <- NULL
  # 1 tracelog
  filename <- utils::head(ids, n = 1)
  text <- c(text, paste0("    <logger id=\"tracelog\" fileName=\"",
    filename, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">"))

  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, "        <log idref=\"likelihood\"/>")

  text <- c(text, "        <log idref=\"prior\"/>")

  n <- length(ids)
  for (i in seq(1, n)) {
    id <- ids[i]
    site_model <- site_models[[i]]
    tree_prior <- tree_priors[[i]]
    clock_model <- clock_models[[i]]

    text <- c(text, paste0("        <log idref=\"treeLikelihood.",
      id, "\"/>"))
    text <- c(text, paste0("        <log id=\"TreeHeight.t:", id,
      "\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:",
      id, "\"/>"))

    if (i > 1) {
      text <- c(text, paste0("        <log idref=\"clockRate.c:", id, "\"/>"))
    }

    text <- c(text, beautier::create_beast2_input_loggers_tree_priors(
      id = id, tree_prior = tree_prior))

    # Now three things
    rates <- beautier::create_beast2_input_loggers_rates(id = id, site_model = site_model) # nolint
    freq_parameters <- beautier::create_beast2_input_loggers_freq_parameter(id = id, site_model = site_model) # nolint
    gamma_shape <- beautier::create_beast2_input_loggers_gamma_shape(id = id, site_model = site_model) # nolint
    gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model)) # nolint
    prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model)) # nolint

    if (is_gtr_site_model(site_model)) {
      if (gcc == 0) {
        text <- c(text, rates)
        text <- c(text, freq_parameters)
      } else if (gcc == 1) {
        text <- c(text, freq_parameters)
        text <- c(text, rates)
      } else {
        testit::assert(gcc >= 2)
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
      id = id, clock_model = clock_model))

  }
  text <- c(text, "    </logger>")
  text
}

#' Creates the screenlog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_screenlog <- function() {
  text <- NULL
  text <- c(text, "")
  text <- c(text, "    <logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "        <log idref=\"posterior\"/>")
  text <- c(text, paste0("        <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"))
  text <- c(text, "        <log idref=\"likelihood\"/>")
  text <- c(text, "        <log idref=\"prior\"/>")
  text <- c(text, "    </logger>")
  text
}

#' Creates the tracelog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_treelogs <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  clock_models = create_strict_clock_models(n = length(ids))
) {
  if (length(ids) != length(clock_models)) {
    stop("Must supply as much IDs as clock models")
  }
  text <- NULL
  n <- length(ids)
  for (i in seq(1, n)) {

    id <- ids[i]
    clock_model <- clock_models[[i]]

    text <- c(text, "")
    text <- c(text, paste0("    <logger id=\"treelog.t:", id, "\" ",
      "fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">"))

    # Clock models
    if (is_strict_clock_model(clock_model)) {
      text <- c(text, paste0("        <log ",
        "id=\"TreeWithMetaDataLogger.t:", id, "\" ",
        "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
        "tree=\"@Tree.t:", id, "\"/>"))
    } else if (is_rln_clock_model(clock_model)) {
      text <- c(text, paste0("        <log ",
        "id=\"TreeWithMetaDataLogger.t:", id, "\" ",
        "spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" ",
        "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
        "tree=\"@Tree.t:", id, "\"/>"))
    }
    text <- c(text, "    </logger>")
  } # next i
  text
}

#' Creates the tree priors part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param id the id of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_tree_priors <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  tree_priors
) {
  text <- NULL
  if (is_yule_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"YuleModel.t:",
      id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"birthRate.t:",
      id, "\"/>"))
  } else if (is_bd_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"BirthDeath.t:",
      id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDBirthRate.t:",
      id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"BDDeathRate.t:",
      id, "\"/>"))
  } else if (is_ccp_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log idref=\"popSize.t:",
      id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"CoalescentConstant.t:",
      id, "\"/>"))
  } else if (is_cbs_tree_prior(tree_priors)) {
    text <- c(text, paste0("        <log ",
      "idref=\"BayesianSkyline.t:", id, "\"/>"))
    text <- c(text, paste0("        <log ",
      "idref=\"bPopSizes.t:", id, "\"/>"))
    text <- c(text, paste0("        <log ",
      "idref=\"bGroupSizes.t:", id, "\"/>"))
  }
  text
}

#' Creates the first site models part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param id the id of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL

  if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("        <log idref=\"kappa.s:", id, "\"/>"))
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("        <log idref=\"kappa1.s:", id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"kappa2.s:", id, "\"/>"))
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("        <log idref=\"rateAC.s:", id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateAG.s:", id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateAT.s:", id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateCG.s:", id, "\"/>"))
    text <- c(text, paste0("        <log idref=\"rateGT.s:", id, "\"/>"))
  }

  text
}

#' Creates the freqParameter part of the log sections
#'   of a BEAST2 XML parameter file
#' @param id the id of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @inheritParams create_beast2_input_loggers
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_freq_parameter <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (!is_jc69_site_model(site_model)) {
    text <- c(text, paste0("        <log ",
      "idref=\"freqParameter.s:", id, "\"/>"))
  }
  text
}

#' Creates the gammaShape part of the log sections
#'   of a BEAST2 XML parameter file
#' @param id the id of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_gamma_shape <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (get_gamma_cat_count(get_gamma_site_model(site_model)) > 1) {
    text <- c(text, paste0("        <log idref=\"gammaShape.s:", id, "\"/>"))
  }
  text
}

#' Creates the clock models part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param id the id of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_file_base_sans_ext}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @export
create_beast2_input_loggers_clock_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model
) {
  text <- NULL
  if (is_rln_clock_model(clock_model)) {
    text <- c(text, paste0("        <log idref=\"ucldStdev.c:", id, "\"/>"))
    text <- c(text, paste0("        <log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>"))
  }
  text
}
