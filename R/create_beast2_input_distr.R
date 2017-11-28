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
      ids = ids,
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
  ids,
  site_models,
  clock_models,
  tree_priors
) {
  text <- NULL
  text <- c(text,
    "        <distribution id=\"prior\" spec=\"util.CompoundDistribution\">")

  # Lines starting with '<distribution id ='
  distr_text <- create_beast2_input_distr_prior_distr(
    ids = ids,
    tree_priors = tree_priors
  )

  # Lines starting with '<prior id ='
  prior_text <- create_beast2_input_distr_prior_prior(
    ids = ids,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors
  )

  # Lines must be mixed sometimes ...
  text <- c(text, distr_text)
  text <- c(text, prior_text)

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
        "        <distribution id=\"likelihood\" ",
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

    text <- c(text, paste0("            <distribution id=\"treeLikelihood.",
      id, "\" spec=\"ThreadedTreeLikelihood\" data=\"@", id,
      "\" tree=\"@Tree.t:", id, "\">"))
    # gamma category count
    gamma_category_count <- beautier::get_gamma_cat_count(
      beautier::get_gamma_site_model(site_model))
    if (gamma_category_count == 0) {
      text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\">")
      )
    } else if (gamma_category_count == 1) {
      text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
        "\">")
      )
    } else {
      text <- c(text, paste0("                <siteModel id=\"SiteModel.s:",
        id, "\" spec=\"SiteModel\" gammaCategoryCount=\"", gamma_category_count,
        "\" shape=\"@gammaShape.s:", id, "\">")
      )
    }


    text <- c(text, paste0("                    <parameter ",
      "id=\"mutationRate.s:", id,
      "\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>"))
    if (gamma_category_count < 2) {
      text <- c(text, paste0("                    <parameter ",
        "id=\"gammaShape.s:", id,
        "\" estimate=\"false\" name=\"shape\">1.0</parameter>"))
    }

    # proportionInvariant
    text <- c(text, paste0(
      "                    <parameter id=\"proportionInvariant.s:",
      id, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"proportionInvariant\" upper=\"1.0\">",
      beautier::get_prop_invariant(
        beautier::get_gamma_site_model(site_model)
      ),
      "</parameter>"))

    text <- c(text,
      create_beast2_input_distr_subst_model(
        id = id,
        site_model = site_model
      )
    )

    text <- c(text, "                </siteModel>")

    # Clock models
    if (i == 1) {
      text <- c(text,
        create_beast2_input_distr_clock_model_first(
          id = id, clock_model = clock_model
        )
      )
    } else {
      text <- c(text,
        create_beast2_input_distr_clock_model_other(
          id = id,
          clock_model = clock_model
        )
      )
    }

    text <- c(text, "            </distribution>")
  }
  text <- c(text, "        </distribution>")

  text
}


#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams create_beast2_input_distr
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_distr <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ids,
  tree_priors = create_yule_tree_priors(ids = ids)
) {
  testit::assert(length(ids) == length(tree_priors))

  text <- NULL
  n <- length(ids)
  for (i in seq(n)) {
    id <- ids[i]
    tree_prior <- tree_priors[[i]]
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
  ids,
  site_models,
  clock_models,
  tree_priors
) {
  text <- NULL
  n <- length(ids)
  for (i in seq(1, n)) {
    id <- ids[i]
    site_model <- site_models[[i]]
    clock_model <- clock_models[[i]]
    tree_prior <- tree_priors[[i]]
    testit::assert(beautier::is_clock_model(clock_model))

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

    # No beautier:: before create_beast2_input_distr_prior_prior_tree_prior, as it is private # nolint
    tree_priors_text <- create_beast2_input_distr_prior_prior_tree_prior(id = id, tree_prior = tree_prior) # nolint
    site_models_text <- create_beast2_input_distr_prior_prior_site_model(id = id, site_model = site_model, i = i) # nolint
    gamma_site_models_text <- create_beast2_input_distr_gamma_site_models(id = id, site_model = site_model) # nolint
    clock_models_text <- create_beast2_input_distr_clock_models(id = id, clock_model = clock_model) # nolint
    prop_invariant <- beautier::get_prop_invariant(get_gamma_site_model(site_model)) # nolint

    # Mix text
    text <- c(text, tree_priors_text)
    if (prop_invariant == get_default_prop_invariant()) {
      text <- c(text, site_models_text)
      text <- c(text, gamma_site_models_text)
    } else {
      text <- c(text, gamma_site_models_text)
      text <- c(text, site_models_text)
    }
    text <- c(text, clock_models_text)
  }
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param tree_prior a tree_prior, as created by \code{\link{create_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  tree_prior
) {
  text <- NULL

  if (is_yule_tree_prior(tree_prior)) {

    text <- c(text,
      indent(
        create_beast2_input_distr_prior_prior_tree_prior_yule(
          yule_tree_prior = tree_prior,
          id = id
        ),
        n_spaces = 12
      )
    )

  } else if (is_bd_tree_prior(tree_prior)) {

    text <- c(text,
      indent(
        create_beast2_input_distr_prior_prior_tree_prior_bd(
          bd_tree_prior = tree_prior,
          id = id
        ),
        n_spaces = 12
      )
    )

  } else if (is_ccp_tree_prior(tree_prior)) {

    text <- c(text,
      indent(
        create_beast2_input_distr_prior_prior_tree_prior_ccp(
          ccp_tree_prior = tree_prior,
          id = id
        ),
        n_spaces = 12
      )
    )

  } else if (is_cep_tree_prior(tree_prior)) {

    text <- c(text,
      indent(
        create_beast2_input_distr_prior_prior_tree_prior_cep(
          cep_tree_prior = tree_prior,
          id = id
        ),
        n_spaces = 12
      )
    )
  }
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a Birth-Death tree prior
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param bd_tree_prior a Birth-Death tree_prior,
#'   as created by \code{\link{create_bd_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_bd <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  bd_tree_prior
) {
  text <- NULL

  # BDBirthRate
  bd_birth_rate_distr <- beautier::get_bd_birth_rate_distr(
    bd_tree_prior = bd_tree_prior)

  text <- c(text,
    create_beast2_input_distr_prior_prior_tree_prior_bd_birth_rate(
      bd_birth_rate_distr = bd_birth_rate_distr,
      id = id
    )
  )
  # BDDeathRate
  bd_death_rate_distr <- beautier::get_bd_death_rate_distr(
    bd_tree_prior = bd_tree_prior)

  text <- c(text,
    create_beast2_input_distr_prior_prior_tree_prior_bd_death_rate(
      bd_death_rate_distr = bd_death_rate_distr,
      id = id
    )
  )

  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a
#' Coalescent Constant Population tree prior
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as created by \code{\link{create_ccp_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_ccp <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  ccp_tree_prior
) {
  # pop size
  create_beast2_input_distr_prior_prior_tree_prior_ccp_pop_size(
    ccp_pop_size_distr = get_ccp_pop_size_distr(
      ccp_tree_prior = ccp_tree_prior
    ),
    id = id
  )
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a
#' Coalescent Exponential Population tree prior
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param cep_tree_prior a Coalescent Exponential Population tree prior,
#'   as created by \code{\link{create_cep_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_cep <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  cep_tree_prior
) {
  text <- NULL
  text <- c(
    text,
    create_beast2_input_distr_prior_prior_tree_prior_cep_pop_size(
      cep_pop_size_distr = get_cep_pop_size_distr(
        cep_tree_prior = cep_tree_prior
      ),
      id = id
    )
  )
  text <- c(
    text,
    create_beast2_input_distr_prior_prior_tree_prior_cep_growth_rate(
      cep_growth_rate_distr = get_cep_growth_rate_distr(
        cep_tree_prior = cep_tree_prior
      ),
      id = id
    )
  )
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a Yule tree prior
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param yule_tree_prior a Yule tree_prior,
#'   as created by \code{\link{create_yule_tree_prior}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_yule <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  yule_tree_prior
) {
  testit::assert(beautier::is_yule_tree_prior(yule_tree_prior))
  text <- NULL

  # birth rate
  yule_birth_rate_distr <- beautier::get_yule_birth_rate_distr(
    yule_tree_prior = yule_tree_prior)

  text <- c(
    text,
    create_beast2_input_distr_prior_prior_tree_prior_yule_birth_rate(
      yule_birth_rate_distr = yule_birth_rate_distr,
      id = id
    )
  )
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' for a Birth-Death tree prior
#' @param bd_birth_rate_distr a Birth-Death birth rate distribution,
#'   as created by \code{\link{create_distr}}
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @usage
#' create_beast2_input_distr_prior_prior_tree_prior_bd_birth_rate(
#'   bd_birth_rate_distr,
#'   id
#' )
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_bd_birth_rate <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  bd_birth_rate_distr,
  id
) {
  text <- NULL
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
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' for a Birth-Death tree prior
#' @param bd_death_rate_distr a Birth-Death death rate distribution,
#'   as created by \code{\link{create_distr}}
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @usage
#' create_beast2_input_distr_prior_prior_tree_prior_bd_death_rate(
#'   bd_death_rate_distr,
#'   id
#' )
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_bd_death_rate <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  bd_death_rate_distr,
  id
) {
  text <- NULL
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
#' of a BEAST2 XML parameter file
#' for a Coalescent Constant Population tree prior
#' @param ccp_pop_size_distr a Coalescent Constant Population
#'   population size distribution,
#'   as created by \code{\link{create_distr}}
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @usage
#' create_beast2_input_distr_prior_prior_tree_prior_ccp_pop_size(
#'   ccp_pop_size_distr,
#'   id
#' )
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_ccp_pop_size <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  ccp_pop_size_distr,
  id
) {
  text <- NULL
  text <- c(text, paste0(
    "<prior id=\"PopSizePrior.t:", id,
    "\" name=\"distribution\" x=\"@popSize.t:",
    id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = ccp_pop_size_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' for a Coalescent Exponential Population tree prior
#' @param cep_pop_size_distr a Coalescent Exponential Population
#'   population size distribution,
#'   as created by \code{\link{create_distr}}
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @usage
#' create_beast2_input_distr_prior_prior_tree_prior_cep_pop_size(
#'   cep_pop_size_distr,
#'   id
#' )
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_cep_pop_size <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  cep_pop_size_distr,
  id
) {
  text <- NULL
  text <- c(text, paste0("<prior ",
    "id=\"ePopSizePrior.t:", id, "\" name=\"distribution\" ",
    "x=\"@ePopSize.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = cep_pop_size_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' for a Coalescent Exponential Population tree prior
#' @param cep_growth_rate_distr a Coalescent Exponential Population
#'   growth rate distribution,
#'   as created by \code{\link{create_distr}}
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @usage
#' create_beast2_input_distr_prior_prior_tree_prior_cep_growth_rate(
#'   cep_growth_rate_distr,
#'   id
#' )
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_cep_growth_rate <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  cep_growth_rate_distr,
  id
) {
  text <- NULL
  text <- c(text, paste0("<prior ",
    "id=\"GrowthRatePrior.t:", id, "\" name=\"distribution\" ",
    "x=\"@growthRate.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = cep_growth_rate_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}


#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' for a Yule tree prior
#' @param yule_birth_rate_distr a Yule birth rate distribution,
#'   as created by \code{\link{create_distr}}
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @usage
#' create_beast2_input_distr_prior_prior_tree_prior_yule_birth_rate(
#'   yule_birth_rate_distr,
#'   id
#' )
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_tree_prior_yule_birth_rate <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  yule_birth_rate_distr,
  id
) {
  text <- NULL
  text <- c(text, paste0("<prior id=\"YuleBirthRatePrior.t:",
    id, "\" name=\"distribution\" x=\"@birthRate.t:", id, "\">"))
  text <- c(text,
    indent(
      distr_to_xml(
        distr = yule_birth_rate_distr
      ),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}


#' Creates the site models section in the priotr section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @param i the ith tree prior
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_prior_prior_site_model <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model,
  i
) {
  text <- NULL
  if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("<prior ",
      "id=\"KappaPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@kappa.s:", id, "\">"))
    text <- c(text,
      beautier::indent(
        beautier::distr_to_xml(site_model$kappa_prior),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
    text <- beautier::indent(text, n_spaces = 12)
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("<prior id=\"kappa1Prior.s:", id, "\" ",
      "name=\"distribution\" x=\"@kappa1.s:", id, "\">"))
    text <- c(text,
      beautier::indent(
        beautier::distr_to_xml(site_model$kappa_1_prior),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
    text <- c(text, paste0("<prior id=\"kappa2Prior.s:", id, "\" ",
      "name=\"distribution\" x=\"@kappa2.s:", id, "\">"))
    text <- c(text,
      beautier::indent(
        beautier::distr_to_xml(site_model$kappa_2_prior),
        n_spaces = 4
      )
    )
    text <- c(text, paste0("</prior>"))
    text <- beautier::indent(text, n_spaces = 12)
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("<prior id=\"RateACPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateAC.s:", id, "\">"))
    text <- c(text, beautier::indent(
      beautier::distr_to_xml(site_model$rate_ac_prior_distr), n_spaces = 4))
    text <- c(text, paste0("</prior>"))
    text <- c(text, paste0("<prior id=\"RateAGPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateAG.s:", id, "\">"))
    text <- c(text, beautier::indent(
      beautier::distr_to_xml(site_model$rate_ag_prior_distr), n_spaces = 4))
    text <- c(text, paste0("</prior>"))
    text <- c(text, paste0("<prior id=\"RateATPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateAT.s:", id, "\">"))
    text <- c(text, beautier::indent(
      beautier::distr_to_xml(site_model$rate_at_prior_distr), n_spaces = 4))
    text <- c(text, paste0("</prior>"))
    text <- c(text, paste0("<prior id=\"RateCGPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateCG.s:", id, "\">"))
    text <- c(text, beautier::indent(
      beautier::distr_to_xml(site_model$rate_cg_prior_distr), n_spaces = 4))
    text <- c(text, paste0("</prior>"))
    text <- c(text, paste0("<prior id=\"RateGTPrior.s:", id, "\" ",
      "name=\"distribution\" x=\"@rateGT.s:", id, "\">"))
    text <- c(text, beautier::indent(
      beautier::distr_to_xml(site_model$rate_gt_prior_distr), n_spaces = 4))
    text <- c(text, paste0("</prior>"))
    text <- beautier::indent(text, n_spaces = 12)
  }
  text
}

#' Creates the gamma site models section in the distribution section
#' of a BEAST2 XML parameter file
#' @param id alignment ID
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_gamma_site_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
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
    text <- beautier::indent(text, n_spaces = 12)
  }
  text

}

#' Creates the clock models section in the distribution section
#' of a BEAST2 XML parameter file
#' @param id alignment ID
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_clock_models <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model
) {
  testit::assert(beautier::is_clock_model(clock_model))
  text <- clock_model_to_prior_xml(id = id, clock_model = clock_model) # nolint internal function call
  if (!is.null(text)) {
    text <- beautier::indent(text, n_spaces = 12)
  }
  text
}

#' Creates the substModel section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_distr
#' @param id alignment ID
#' @param site_model a site_model, as created by \code{\link{create_site_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_subst_model <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  site_model
) {
  text <- NULL
  if (beautier::is_jc69_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"JC69.s:", id, "\" spec=\"JukesCantor\"/>"))
    text <- beautier::indent(text, n_spaces = 20)
  } else if (is_hky_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"hky.s:", id, "\" spec=\"HKY\" kappa=\"@kappa.s:", id, "\">"))
    text <- c(text, paste0("    <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("</substModel>"))
    text <- beautier::indent(text, n_spaces = 20)
  } else if (is_tn93_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"tn93.s:", id, "\" spec=\"TN93\" kappa1=\"@kappa1.s:", id, "\" ",
      "kappa2=\"@kappa2.s:", id, "\">"))
    text <- c(text, paste0("    <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("</substModel>"))
    text <- beautier::indent(text, n_spaces = 20)
  } else if (is_gtr_site_model(site_model)) {
    text <- c(text, paste0("<substModel ",
      "id=\"gtr.s:", id, "\" spec=\"GTR\" rateAC=\"@rateAC.s:", id, "\" ",
      "rateAG=\"@rateAG.s:", id, "\" rateAT=\"@rateAT.s:", id, "\" ",
      "rateCG=\"@rateCG.s:", id, "\" rateGT=\"@rateGT.s:", id, "\">"))
    text <- c(text, paste0("    <parameter ",
      "id=\"rateCT.s:", id, "\" estimate=\"false\" lower=\"0.0\" ",
      "name=\"rateCT\">1.0</parameter>"))
    text <- c(text, paste0("    <frequencies ",
      "id=\"estimatedFreqs.s:", id, "\" spec=\"Frequencies\" ",
      "frequencies=\"@freqParameter.s:", id, "\"/>"))
    text <- c(text, paste0("</substModel>"))
    text <- beautier::indent(text, n_spaces = 20)
  }
  text
}

#' Creates the first clock models' section in the distribution section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_clock_model_first <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model
) {
  text <- clock_model_to_brm_xml(id = id, clock_model = clock_model) # nolint internal function call
  if (!is.null(text)) {
    text <- beautier::indent(text, n_spaces = 16)
  }
  text
}

#' Creates the second or later clock models' section in the distribution section
#' of a BEAST2 XML parameter file
#' @param id the ID of the alignment (can be extracted from
#'   its FASTA filesname using \code{\link{get_id}})
#' @param clock_model a clock_model,
#'   as created by \code{\link{create_clock_model}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
create_beast2_input_distr_clock_model_other <- function( # nolint long function name is fine, as (1) it follows a pattern (2) this function is not intended to be used regularily
  id,
  clock_model
) {
  text <- clock_model_to_other_brm_xml(id = id, clock_model = clock_model) # nolint internal function call
  if (!is.null(text)) {
    text <- beautier::indent(text, n_spaces = 16)
  }
  text
}
