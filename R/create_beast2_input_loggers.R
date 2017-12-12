#' Creates the two logger sections of a BEAST2 XML parameter file
#' @author Richel J.C. Bilderbeek
#' @inheritParams default_params_doc
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_jc69_site_models(ids = ids),
  clock_models = create_strict_clock_models(ids = ids),
  tree_priors = create_yule_tree_priors(ids = ids)
) {
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(beautier::are_ids(ids))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL
  text <- c(text, create_beast2_input_tracelog(
    ids = ids,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors)
  )

  text <- c(text, "")

  text <- c(text,
    indent(
      create_beast2_input_screenlog(),
      n_spaces = 4
    )
  )

  text <- c(text,
    indent(
      create_beast2_input_treelogs(clock_models),
      n_spaces = 4
    )
  )

  text
}


#' Creates the tracelog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
create_beast2_input_tracelog <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_jc69_site_models(ids = ids),
  clock_models = create_strict_clock_models(ids = ids),
  tree_priors = create_yule_tree_priors(ids = ids)
) {
  testit::assert(beautier::are_ids(ids))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) >= length(clock_models))
  testit::assert(length(ids) == length(tree_priors))

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

    text <- c(text,
      indent(
        create_beast2_input_loggers_tree_prior(
          tree_prior = tree_prior
        ),
        n_spaces = 8
      )
    )

    # Now three things
    rates <- create_beast2_input_loggers_rates(site_model = site_model) # nolint
    freqparams <- create_beast2_input_loggers_freqparam(site_model = site_model) # nolint
    gamma_shape <- create_beast2_input_loggers_gamma_shape(site_model = site_model) # nolint
    gcc <- beautier::get_gamma_cat_count(beautier::get_gamma_site_model(site_model)) # nolint
    prop_invariant <- beautier::get_prop_invariant(beautier::get_gamma_site_model(site_model)) # nolint

    if (is_gtr_site_model(site_model)) {
      if (gcc == 0) {
        text <- c(text, rates)
        text <- c(text, freqparams)
      } else if (gcc == 1) {
        text <- c(text, freqparams)
        text <- c(text, rates)
      } else {
        testit::assert(gcc >= 2)
        if (prop_invariant == get_default_prop_invariant()) {
          text <- c(text, freqparams)
          text <- c(text, rates)
          text <- c(text, gamma_shape)
        } else {
          text <- c(text, gamma_shape)
          text <- c(text, freqparams)
          text <- c(text, rates)
        }
      }
    } else {
      text <- c(text, rates)
      text <- c(text, gamma_shape)
      text <- c(text, freqparams)
    }



    text <- c(text,
      create_beast2_input_loggers_clock_models(
        clock_model = clock_model
      )
    )

  }
  text <- c(text, "    </logger>")
  text
}

#' Creates the screenlog section of the logger section
#' of a BEAST2 XML parameter file
#' @return the XML text
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
create_beast2_input_screenlog <- function() {
  text <- NULL
  text <- c(text, "<logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "    <log idref=\"posterior\"/>")
  text <- c(text, paste0("    <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"))
  text <- c(text, "    <log idref=\"likelihood\"/>")
  text <- c(text, "    <log idref=\"prior\"/>")
  text <- c(text, "</logger>")
  text
}

#' Creates the tracelog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
create_beast2_input_treelogs <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  clock_models
) {
  testit::assert(beautier::are_clock_models(clock_models))

  text <- NULL
  for (clock_model in clock_models) {

    text <- c(text, "")
    id <- clock_model$id
    text <- c(text, paste0("<logger id=\"treelog.t:", id, "\" ",
      "fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">"))
    text <- c(
      text,
      indent(clock_model_to_xml_treelogger(clock_model), n_spaces = 4)
    )
    text <- c(text, "</logger>")
  }
  text
}

#' Creates the tree priors part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @param tree_prior a tree prior,
#'   as created by \code{\link{create_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_loggers_tree_prior <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_prior
) {
  testit::assert(beautier::is_tree_prior(tree_prior))
  id <- tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (is_yule_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"YuleModel.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"birthRate.t:", id, "\"/>"))
  } else if (is_bd_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"BirthDeath.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"BDBirthRate.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"BDDeathRate.t:", id, "\"/>"))
  } else if (is_ccp_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"popSize.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"CoalescentConstant.t:", id, "\"/>"))
  } else if (is_cbs_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"BayesianSkyline.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"bPopSizes.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"bGroupSizes.t:", id, "\"/>"))
  } else if (is_cep_tree_prior(tree_prior)) {
    text <- c(text, paste0("<log idref=\"CoalescentExponential.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"ePopSize.t:", id, "\"/>"))
    text <- c(text, paste0("<log idref=\"growthRate.t:", id, "\"/>"))
  }
  text
}

#' Creates the first site models part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_loggers_rates <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

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
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_loggers_freqparam <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (!is_jc69_site_model(site_model)) {
    text <- c(text, paste0("        <log ",
      "idref=\"freqParameter.s:", id, "\"/>"))
  }
  text
}

#' Creates the gammaShape part of the log sections
#'   of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_loggers_gamma_shape <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (get_gamma_cat_count(get_gamma_site_model(site_model)) > 1) {
    text <- c(text, paste0("        <log idref=\"gammaShape.s:", id, "\"/>"))
  }
  text
}

#' Creates the clock models part of the two logger sections
#'   of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_loggers_clock_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_rln_clock_model(clock_model)) {
    text <- c(text, paste0("<log idref=\"ucldStdev.c:", id, "\"/>"))
    text <- c(text, paste0("<log id=\"rate.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.RateStatistic\" ",
      "branchratemodel=\"@RelaxedClock.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\"/>"))
  }
  indent(text, n_spaces = 8) # nolint internal function
}
