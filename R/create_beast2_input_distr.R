#' Creates the distribution section of a BEAST2 XML parameter file.
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_id}})
#' @inheritParams create_beast2_input
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @seealso \code{\link{create_beast2_input}}
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models,
  clock_models,
  tree_priors
) {
  testit::assert(beautier::are_ids(ids))
  testit::assert(length(ids) >= length(site_models))
  testit::assert(length(ids) >= length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))
  testit::assert(are_init_tree_priors(tree_priors)) # nolint internal function call

  text <- NULL
  text <- c(text,
    "    <distribution id=\"posterior\" spec=\"util.CompoundDistribution\">")

  # prior
  text <- c(text, create_beast2_input_distr_prior(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors
    )
  )

  # likelihood
  text <- c(text, create_beast2_input_distr_likelihood(
      ids = ids,
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors
    )
  )

  text <- c(text, "    </distribution>") # posterior distribution
  text
}


#' Creates the prior section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distr
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_models,
  clock_models,
  tree_priors
) {
  text <- NULL
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  # Lines starting with '<distribution id ='
  text <- c(
    text,
    create_beast2_input_distr_prior_distr(
      tree_priors = tree_priors
    )
  )

  # Lines starting with '<prior id ='
  text <- c(
    text,
    create_beast2_input_distr_prior_prior(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors
    )
  )

  text <- c(text, "        </distribution>")
  text
}


#' Creates the likelihood section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distr
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_likelihood <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  site_models,
  clock_models,
  tree_priors
) {
  text <- NULL
  text <- c(
      text,
      paste0(
        "<distribution id=\"likelihood\" ",
        "spec=\"util.CompoundDistribution\" useThreads=\"true\">"
      )
    )

  n <- length(ids)
  for (i in seq(1, n)) {
    id <- ids[i]
    site_model <- site_models[[i]]
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))
    testit::assert(beautier::is_site_model(site_model))

    text <- c(text, paste0("    <distribution id=\"treeLikelihood.",
      id, "\" spec=\"ThreadedTreeLikelihood\" data=\"@", id,
      "\" tree=\"@Tree.t:", id, "\">"))
    # gamma category count
    gamma_category_count <- beautier::get_gamma_cat_count(
      beautier::get_gamma_site_model(site_model))
    if (gamma_category_count == 0) {
      text <- c(text, paste0("        <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\">")
      )
    } else if (gamma_category_count == 1) {
      text <- c(text, paste0("        <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
        "\">")
      )
    } else {
      text <- c(text, paste0("        <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
        "\" shape=\"@gammaShape.s:", id, "\">")
      )
    }


    text <- c(text, paste0("            <parameter ",
      "id=\"mutationRate.s:", id,
      "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
    if (gamma_category_count < 2) {
      text <- c(text, paste0("            <parameter ",
        "id=\"gammaShape.s:", id,
        "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
    }

    # proportionInvariant
    text <- c(text, paste0(
      "            <parameter id=\"proportionInvariant.s:",
      id, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"proportionInvariant\" upper=\"1.0\">",
      beautier::get_prop_invariant(
        beautier::get_gamma_site_model(site_model)
      ),
      "</parameter>"))

    text <- c(text,
      beautier::indent(
        site_model_to_xml_subst_model(site_model),
        n_spaces = 12
      )
    )

    text <- c(text, "        </siteModel>")

    # Clock models
    if (i == 1) {
      text <- c(text,
        beautier::indent(
          clock_model_to_xml_brm(clock_model),
          n_spaces = 8
        )
      )
    } else {
      text <- c(text,
        beautier::indent(
          clock_model_to_xml_brm_nonfirst(clock_model),
          n_spaces = 8
        )
      )
    }

    text <- c(text, "    </distribution>")
  }
  text <- c(text, "</distribution>")

  beautier::indent(text, n_spaces = 8)
}


#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams create_beast2_input_distr
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_distr <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  tree_priors
) {

  text <- NULL
  for (tree_prior in tree_priors) {
    id <- tree_prior$id
    if (is_yule_tree_prior(tree_prior)) {
      text <- c(text, paste0("            <distribution id=\"YuleModel.t:", id,
        "\" spec=\"beast.evolution.speciation.YuleModel\" ",
        "birthDiffRate=\"@birthRate.t:", id, "\" tree=\"@Tree.t:", id, "\"/>"))
    } else if (is_bd_tree_prior(tree_prior)) {
      text <- c(text, paste0("            <distribution id=\"BirthDeath.t:", id,
        "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" ",
        "birthDiffRate=\"@BDBirthRate.t:", id, "\" ",
        "relativeDeathRate=\"@BDDeathRate.t:", id, "\" ",
        "tree=\"@Tree.t:", id, "\"/>"))

    } else if (is_ccp_tree_prior(tree_prior)) {
      text <- c(text, paste0("            ",
        "<distribution id=\"CoalescentConstant.t:", id,
        "\" spec=\"Coalescent\">"))
      text <- c(text, paste0("                ",
        "<populationModel id=\"ConstantPopulation.t:", id,
        "\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:", id, "\"/>"))
      text <- c(text, paste0(
        "                <treeIntervals id=\"TreeIntervals.t:",
        id, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
        id, "\"/>"))
      text <- c(text, "            </distribution>")
    } else if (is_cbs_tree_prior(tree_prior)) {
      text <- c(text, paste0("            <distribution ",
        "id=\"BayesianSkyline.t:",
        id, "\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:", id,
        "\" popSizes=\"@bPopSizes.t:", id, "\">"))
      text <- c(text, paste0("                ",
        "<treeIntervals id=\"BSPTreeIntervals.t:", id, "\" ",
        "spec=\"TreeIntervals\" tree=\"@Tree.t:", id, "\"/>"))
      text <- c(text, paste0("            </distribution>"))
      text <- c(text, paste0("            ",
        "<distribution id=\"MarkovChainedPopSizes.t:", id,
        "\" spec=\"beast.math.distributions.MarkovChainDistribution\" ",
        "jeffreys=\"true\" parameter=\"@bPopSizes.t:", id, "\"/>"))
    } else if (is_cep_tree_prior(tree_prior)) {
      text <- c(text, paste0("            <distribution ",
        "id=\"CoalescentExponential.t:", id, "\" spec=\"Coalescent\">"))
      text <- c(text, paste0("                <populationModel ",
        "id=\"ExponentialGrowth.t:", id, "\" spec=\"ExponentialGrowth\" ",
        "growthRate=\"@growthRate.t:", id, "\" ",
        "popSize=\"@ePopSize.t:", id, "\"/>"))
      text <- c(text, paste0("                <treeIntervals ",
        "id=\"TreeIntervals.t:", id, "\" spec=\"TreeIntervals\" ",
        "tree=\"@Tree.t:", id, "\"/>"))
      text <- c(text, paste0("            </distribution>"))
    }
  }
  text
}


#' Creates the prior section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<prior id='
#' @inheritParams create_beast2_input_distr
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_models,
  clock_models,
  tree_priors
) {
  text <- NULL

  for (i in seq_along(tree_priors)) {
    tree_prior <- tree_priors[[i]]
    id <- tree_prior$id
    # Irregularity: WIP, TODO, to be moved to someplace else
    if (is_yule_tree_prior(tree_prior)) {

      if (i == 2) {
        text <- c(text, paste0("            <prior ",
          "id=\"ClockPrior.c:", id, "\" name=\"distribution\" ",
          "x=\"@clockRate.c:", id, "\">"))
        text <- c(text, paste0("                <Uniform id=\"Uniform.3\" ",
          "name=\"distr\" upper=\"Infinity\"/>"))
        text <- c(text, paste0("            </prior>"))
      }
    }

    text <- c(
      text,
      beautier::indent(
        tree_prior_to_xml_prior(tree_prior),
        n_spaces = 12
      )
    )
  }

  for (i in seq_along(site_models)) {
    site_model <- site_models[[i]]
    id <- site_model$id

    site_models_text <- beautier::indent(site_model_to_xml_prior(site_model), n_spaces = 12) # nolint
    gamma_site_models_text <- create_beast2_input_distr_gamma_site_models(site_model = site_model) # nolint
    prop_invariant <- beautier::get_prop_invariant(get_gamma_site_model(site_model)) # nolint

    # Mix text
    if (prop_invariant == get_default_prop_invariant()) {
      text <- c(text, site_models_text)
      text <- c(text, gamma_site_models_text)
    } else {
      text <- c(text, gamma_site_models_text)
      text <- c(text, site_models_text)
    }
  }

  for (clock_model in clock_models) {
    text <- c(
      text,
      beautier::indent(
        clock_model_to_xml_prior(clock_model),
        n_spaces = 12
      )
    )
  }
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a Birth-Death tree prior
#' @param bd_tree_prior a Birth-Death tree_prior,
#'   as created by \code{\link{create_bd_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
bd_tree_prior_to_xml_prior <- function(
  bd_tree_prior
) {
  testit::assert(beautier::is_bd_tree_prior(bd_tree_prior))
  id <- bd_tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL

  # BDBirthRate
  bd_birth_rate_distr <- beautier::get_bd_birth_rate_distr(
    bd_tree_prior = bd_tree_prior)

  text <- c(text, paste0("<prior id=\"BirthRatePrior.t:", id,
    "\" name=\"distribution\" x=\"@BDBirthRate.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = bd_birth_rate_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))

  # BDDeathRate
  bd_death_rate_distr <- beautier::get_bd_death_rate_distr(
    bd_tree_prior = bd_tree_prior)

  text <- c(text, paste0("<prior id=\"DeathRatePrior.t:", id,
    "\" name=\"distribution\" x=\"@BDDeathRate.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = bd_death_rate_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))

  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a
#' Coalescent Constant Population tree prior
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as created by \code{\link{create_ccp_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_ccp <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ccp_tree_prior
) {
  testit::assert(beautier::is_ccp_tree_prior(ccp_tree_prior))
  id <- ccp_tree_prior$id
  testit::assert(beautier::is_id(id))

  # pop size
  text <- NULL
  text <- c(text, paste0(
    "<prior id=\"PopSizePrior.t:", id,
    "\" name=\"distribution\" x=\"@popSize.t:",
    id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = ccp_tree_prior$pop_size_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a
#' Coalescent Exponential Population tree prior
#' @param cep_tree_prior a Coalescent Exponential Population tree prior,
#'   as created by \code{\link{create_cep_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_cep <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  cep_tree_prior
) {
  testit::assert(beautier::is_cep_tree_prior(cep_tree_prior))
  id <- cep_tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0("<prior ",
    "id=\"ePopSizePrior.t:", id, "\" name=\"distribution\" ",
    "x=\"@ePopSize.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = cep_tree_prior$pop_size_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))

  text <- c(text, paste0("<prior ",
    "id=\"GrowthRatePrior.t:", id, "\" name=\"distribution\" ",
    "x=\"@growthRate.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = cep_tree_prior$growth_rate_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}

#' Creates the \code{prior} section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a Yule tree prior
#' @param yule_tree_prior a Yule tree_prior,
#'   as created by \code{\link{create_yule_tree_prior}}
#' @author Richel J.C. Bilderbeek
yule_tree_prior_to_xml_prior <- function(
  yule_tree_prior
) {
  testit::assert(beautier::is_yule_tree_prior(yule_tree_prior))
  id <- yule_tree_prior$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  text <- c(text, paste0(
      "<prior id=\"YuleBirthRatePrior.t:", id, "\" ",
      "name=\"distribution\" x=\"@birthRate.t:", id, "\">"
    )
  )
  text <- c(text,
    indent(
      distr_to_xml(
        distr = yule_tree_prior$birth_rate_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}

#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_gamma_site_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  site_model
) {
  testit::assert(beautier::is_site_model(site_model))
  id <- site_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  gamma_site_model <- beautier::get_gamma_site_model(
    site_model = site_model)
  if (get_gamma_cat_count(gamma_site_model) >= 2) {
    text <- c(text, paste0("<prior ",
      "id=\"GammaShapePrior.s:", id, "\" name=\"distribution\" ",
      "x=\"@gammaShape.s:", id, "\">"))
    text <- c(text, paste0("    <Exponential id=\"Exponential.0\" ",
      "name=\"distr\">"))
    text <- c(text, paste0("        <parameter ",
      "id=\"RealParameter.0\" estimate=\"false\" ",
      "name=\"mean\">1.0</parameter>"))
    text <- c(text, paste0("    </Exponential>"))
    text <- c(text, paste0("</prior>"))
  }
  text <- beautier::indent(text, n_spaces = 12)
  text

}
