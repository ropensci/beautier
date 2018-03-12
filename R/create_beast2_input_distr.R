#' Creates the distribution section of a BEAST2 XML parameter file.
#' @inheritParams default_params_doc
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @seealso \code{\link{create_beast2_input}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
create_beast2_input_distr <- function( # nolint internal function
  site_models,
  clock_models,
  tree_priors,
  mrca_priors = NA
) {
  testit::assert(are_site_models(site_models))
  testit::assert(are_clock_models(clock_models))
  testit::assert(are_tree_priors(tree_priors))
  testit::assert(are_init_tree_priors(tree_priors)) # nolint internal function call
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function call

  text <- NULL

  # prior
  text <- c(
    text,
    create_beast2_input_distr_prior(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      mrca_priors = mrca_priors
    )
  )

  # likelihood
  text <- c(
    text,
    create_beast2_input_distr_lh(
      site_models = site_models,
      clock_models = clock_models,
      mrca_priors = mrca_priors
    )
  )
  text <- indent(text, n_spaces = 4) # nolint internal function
  text <- c(
    "<distribution id=\"posterior\" spec=\"util.CompoundDistribution\">",
    text
  )
  text <- c(text, "</distribution>") # posterior distribution
  text <- indent(text, n_spaces = 4) # nolint internal function
  text
}


#' Creates the prior section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @seealso this function is called by \code{\link{create_beast2_input_distr}},
#'   together with \code{\link{create_beast2_input_distr_lh}}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
create_beast2_input_distr_prior <- function( # nolint internal function
  site_models,
  clock_models,
  tree_priors,
  mrca_priors = NA
) {
  text <- NULL
  text <- c(text, tree_priors_to_xml_prior_distr(tree_priors)) # nolint internal function
  text <- c(text, gamma_site_models_to_xml_prior_distr(site_models)) # nolint internal function
  text <- c(text, site_models_to_xml_prior_distr(site_models)) # nolint internal function
  text <- c(text, mrca_priors_to_xml_prior_distr( # nolint internal function
    mrca_priors,
    has_non_strict_clock_model = get_has_non_strict_clock_model(clock_models))
  )
  text <- c(text, clock_models_to_xml_prior_distr(clock_models)) # nolint internal function

  text <- indent(text, n_spaces = 4) # nolint internal function

  # Surround text by prior distribution tag
  text <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">",
    text)
  text <- c(text, "</distribution>")
}


#' Creates the likelihood section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richel J.C. Bilderbeek
#' @seealso this function is called by \code{\link{create_beast2_input_distr}},
#'   together with \code{\link{create_beast2_input_distr_prior}}
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
create_beast2_input_distr_lh <- function( # nolint internal function
  site_models,
  clock_models,
  mrca_priors = NA
) {
  testit::assert(length(site_models) == length(clock_models))
  testit::assert(!has_shared_rln_clock_models(clock_models)) # nolint internal function

  text <- NULL
  n <- length(site_models)
  for (i in seq(1, n)) {
    site_model <- site_models[[i]]
    clock_model <- clock_models[[i]]
    id <- site_model$id
    is_first <- i == 1
    brm_line <- ""
    j <- get_first_clock_model_index(clock_model, clock_models) # nolint internal function
    if (i != j) {
      testit::assert(j < i)
      branch_rate_model_ref <- clock_models[[j]]$id
      clock_model_str <- get_clock_model_name(clock_models[[j]]) # nolint internal function
      brm_line <- paste0(
        "branchRateModel=\"@", clock_model_str, ".c:",
        branch_rate_model_ref, "\" "
      )
    }
    text <- c(text, paste0("<distribution id=\"treeLikelihood.",
      id, "\" spec=\"ThreadedTreeLikelihood\" ",
      brm_line,
      "data=\"@", id,
      "\" tree=\"@Tree.t:", id, "\">"))
    text <- c(text,
      indent(
        site_model_to_xml_lh_distr(site_model),
        n_spaces = 4
      )
    )

    is_non_first_shared <- is_index_of_non_first_shared_clock_model(i, clock_models) # nolint internal function
    has_mrca_priors <- TRUE
    if (length(mrca_priors) == 1 && is.na(mrca_priors)) {
      has_mrca_priors <- FALSE
    }
    if (!has_mrca_priors || get_has_non_strict_clock_model(clock_models)) {
      text <- c(text,
        indent(
          clock_model_to_xml_lh_distr(
            clock_model,
            is_first = is_first,
            is_non_first_shared = is_non_first_shared
          ),
          n_spaces = 4
        )
      )
    }
    # Can be either NA or a list of 1 element
    testit::assert(are_mrca_priors(mrca_priors))
    testit::assert(length(mrca_priors) >= 1)
    mrca_prior <- NA
    if (!is_one_na(mrca_priors)) mrca_prior <- mrca_priors[[1]] # nolint
    testit::assert(is_mrca_prior(mrca_prior))
    text <- c(text,
      indent(
        mrca_prior_to_xml_lh_distr(
          mrca_prior,
          has_non_strict_clock_model = get_has_non_strict_clock_model(
            clock_models
          )
        ),
        n_spaces = 4
      )
    )
    # Close of '<distribution id="treeLikelihood.test_output_0"...'
    text <- c(text, "</distribution>")
  }


  text <- indent(text, n_spaces = 4) # nolint internal function

  # Surround by likelihood distribution tags
  text <- c(paste0(
    "<distribution id=\"likelihood\" ",
    "spec=\"util.CompoundDistribution\" useThreads=\"true\">"),
    text)
  text <- c(text, "</distribution>")

  # Must have one or zero branchRateModel
  # testit::assert(sum(grepl(x = text, pattern = " *<branchRateModel.*")) < length(site_models)) # nolint perhaps one day

  text
}


#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a Birth-Death tree prior
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
bd_tree_prior_to_xml_prior_distr <- function( # nolint internal function
  bd_tree_prior
) {
  testit::assert(is_bd_tree_prior(bd_tree_prior))
  id <- bd_tree_prior$id
  testit::assert(is_id(id))

  text <- NULL

  text <- c(text, paste0("<distribution id=\"BirthDeath.t:", id,
    "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" ",
    "birthDiffRate=\"@BDBirthRate.t:", id, "\" ",
    "relativeDeathRate=\"@BDDeathRate.t:", id, "\" ",
    "tree=\"@Tree.t:", id, "\"/>"))

  # BDBirthRate
  bd_birth_rate_distr <- bd_tree_prior$birth_rate_distr

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
  bd_death_rate_distr <- bd_tree_prior$death_rate_distr

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
#' of a BEAST2 XML parameter file for a Birth-Death tree prior
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
cbs_tree_prior_to_xml_prior_distr <- function( # nolint internal function
  cbs_tree_prior
) {
  testit::assert(is_cbs_tree_prior(cbs_tree_prior))
  id <- cbs_tree_prior$id
  testit::assert(is_id(id))

  text <- NULL
  text <- c(text, paste0("<distribution ",
    "id=\"BayesianSkyline.t:",
    id, "\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:", id,
    "\" popSizes=\"@bPopSizes.t:", id, "\">"))
  text <- c(text, paste0("    ",
    "<treeIntervals id=\"BSPTreeIntervals.t:", id, "\" ",
    "spec=\"TreeIntervals\" tree=\"@Tree.t:", id, "\"/>"))
  text <- c(text, paste0("</distribution>"))
  text <- c(text, paste0("<distribution id=\"MarkovChainedPopSizes.t:", id,
    "\" spec=\"beast.math.distributions.MarkovChainDistribution\" ",
    "jeffreys=\"true\" parameter=\"@bPopSizes.t:", id, "\"/>"))
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a
#' Coalescent Constant Population tree prior
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
ccp_tree_prior_to_xml_prior_distr <- function( # nolint internal function
  ccp_tree_prior
) {
  testit::assert(is_ccp_tree_prior(ccp_tree_prior))
  id <- ccp_tree_prior$id
  testit::assert(is_id(id))

  text <- NULL

  # distributions
  text <- c(text, paste0("<distribution id=\"CoalescentConstant.t:", id,
    "\" spec=\"Coalescent\">"))
  text <- c(text, paste0("    ",
    "<populationModel id=\"ConstantPopulation.t:", id,
    "\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:", id, "\"/>"))
  text <- c(text, paste0(
    "    <treeIntervals id=\"TreeIntervals.t:",
    id, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
    id, "\"/>"))
  text <- c(text, "</distribution>")

  # pop size
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
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
cep_tree_prior_to_xml_prior_distr <- function( # nolint internal function
  cep_tree_prior
) {
  testit::assert(is_cep_tree_prior(cep_tree_prior))
  id <- cep_tree_prior$id
  testit::assert(is_id(id))

  text <- NULL

  # distribution
  text <- c(text, paste0("<distribution ",
    "id=\"CoalescentExponential.t:", id, "\" spec=\"Coalescent\">"))
  text <- c(text, paste0("    <populationModel ",
    "id=\"ExponentialGrowth.t:", id, "\" spec=\"ExponentialGrowth\" ",
    "growthRate=\"@growthRate.t:", id, "\" ",
    "popSize=\"@ePopSize.t:", id, "\"/>"))
  text <- c(text, paste0("    <treeIntervals ",
    "id=\"TreeIntervals.t:", id, "\" spec=\"TreeIntervals\" ",
    "tree=\"@Tree.t:", id, "\"/>"))
  text <- c(text, paste0("</distribution>"))

  # prior
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
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
yule_tree_prior_to_xml_prior_distr <- function( # nolint internal function
  yule_tree_prior
) {
  testit::assert(is_yule_tree_prior(yule_tree_prior))
  id <- yule_tree_prior$id
  testit::assert(is_id(id))

  text <- NULL

  # distribution
  text <- c(text, paste0("<distribution id=\"YuleModel.t:", id,
    "\" spec=\"beast.evolution.speciation.YuleModel\" ",
    "birthDiffRate=\"@birthRate.t:", id, "\" tree=\"@Tree.t:", id, "\"/>"))

  # prior
  text <- c(text, paste0(
      "<prior id=\"YuleBirthRatePrior.t:", id, "\" ",
      "name=\"distribution\" x=\"@birthRate.t:", id, "\">"
    )
  )
  text <- c(text,
    indent(
      distr_to_xml(yule_tree_prior$birth_rate_distr),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
