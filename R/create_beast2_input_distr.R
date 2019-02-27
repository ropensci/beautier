#' Creates the distribution section of a BEAST2 XML parameter file.
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @seealso \code{\link{create_beast2_input}}
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_distr <- function( # nolint beautier function
  site_models,
  clock_models,
  tree_priors,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(are_site_models(site_models)) # nolint beautier function
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  testit::assert(are_tree_priors(tree_priors)) # nolint beautier function
  testit::assert(are_init_tree_priors(tree_priors)) # nolint beautier function call
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function call

  text <- NULL

  # prior
  text <- c(
    text,
    create_beast2_input_distr_prior(
      site_models = site_models,
      clock_models = clock_models,
      tree_priors = tree_priors,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )

  # likelihood
  text <- c(
    text,
    create_beast2_input_distr_lh(
      site_models = site_models,
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )
  text <- indent(text, n_spaces = 4) # nolint beautier function
  text <- c(
    "<distribution id=\"posterior\" spec=\"util.CompoundDistribution\">",
    text
  )
  text <- c(text, "</distribution>") # posterior distribution
  text <- indent(text, n_spaces = 4) # nolint beautier function
  text
}


#' Creates the prior section in the distribution section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @seealso this function is called by \code{create_beast2_input_distr},
#'   together with \code{create_beast2_input_distr_lh}
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
create_beast2_input_distr_prior <- function( # nolint beautier function
  site_models,
  clock_models,
  tree_priors,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  text <- NULL
  text <- c(text, tree_priors_to_xml_prior_distr(tree_priors)) # nolint beautier function
  text <- c(text, gamma_site_models_to_xml_prior_distr(site_models)) # nolint beautier function
  text <- c(text, site_models_to_xml_prior_distr(site_models)) # nolint beautier function
  text <- c(text, mrca_priors_to_xml_prior_distr( # nolint beautier function
    mrca_priors,
    has_non_strict_clock_model = get_has_non_strict_clock_model(clock_models))
  )
  text <- c(
    text,
    clock_models_to_xml_prior_distr( # nolint beautier function
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )

  text <- indent(text, n_spaces = 4) # nolint beautier function

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
#' @author Richèl J.C. Bilderbeek
#' @seealso this function is called by \code{create_beast2_input_distr},
#'   together with \code{create_beast2_input_distr_prior}
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
create_beast2_input_distr_lh <- function( # nolint beautier function
  site_models,
  clock_models,
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(length(site_models) == 1)
  testit::assert(length(site_models) == length(clock_models))

  text <- NULL
  n <- length(site_models)
  for (i in seq(1, n)) {
    site_model <- site_models[[i]]
    clock_model <- clock_models[[i]]
    id <- site_model$id
    brm_line <- ""
    text <- c(text, paste0("<distribution id=\"treeLikelihood.",
      id, "\" spec=\"ThreadedTreeLikelihood\" ",
      brm_line,
      "data=\"@", id,
      "\" tree=\"@Tree.t:", id, "\">"))
    text <- c(text,
      indent( # nolint beautier function
        site_model_to_xml_lh_distr(site_model), # nolint beautier function
        n_spaces = 4
      )
    )

    if (is_one_na(mrca_priors) || get_has_non_strict_clock_model(clock_models)) { # nolint beautier function
      text <- c(text,
        indent( # nolint beautier function
          clock_model_to_xml_lh_distr( # nolint beautier function
            clock_model,
            mrca_priors = mrca_priors,
            tipdates_filename = tipdates_filename
          ),
          n_spaces = 4
        )
      )
    }
    # Can be either NA or a list of 1 element
    testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
    testit::assert(length(mrca_priors) >= 1)
    mrca_prior <- NA
    if (!is_one_na(mrca_priors)) mrca_prior <- mrca_priors[[1]] # nolint
    testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function
    text <- c(text,
      indent( # nolint beautier function
        mrca_prior_to_xml_lh_distr( # nolint beautier function
          mrca_prior,
          has_non_strict_clock_model = get_has_non_strict_clock_model( # nolint beautier function
            clock_models
          )
        ),
        n_spaces = 4
      )
    )
    # Close of '<distribution id="treeLikelihood.test_output_0"...'
    text <- c(text, "</distribution>")
  }


  text <- indent(text, n_spaces = 4) # nolint beautier function

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
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
bd_tree_prior_to_xml_prior_distr <- function( # nolint beautier function
  bd_tree_prior
) {
  testit::assert(is_bd_tree_prior(bd_tree_prior)) # nolint beautier function
  id <- bd_tree_prior$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL

  text <- c(text, paste0("<distribution id=\"BirthDeath.t:", id,
    "\" spec=\"beast.evolution.speciation.BirthDeathGernhard08Model\" ",
    "birthDiffRate=\"@BDBirthRate.t:", id, "\" ",
    "relativeDeathRate=\"@BDDeathRate.t:", id, "\" ",
    "tree=\"@Tree.t:", id, "\"/>")) # nolint this is no absolute path

  # BDBirthRate
  bd_birth_rate_distr <- bd_tree_prior$birth_rate_distr

  text <- c(text, paste0("<prior id=\"BirthRatePrior.t:", id,
    "\" name=\"distribution\" x=\"@BDBirthRate.t:", id, "\">"))
  text <- c(text,
    indent( # nolint beautier function
      distr_to_xml( # nolint beautier function
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
    indent( # nolint beautier function
      distr_to_xml( # nolint beautier function
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
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
cbs_tree_prior_to_xml_prior_distr <- function( # nolint beautier function
  cbs_tree_prior
) {
  testit::assert(is_cbs_tree_prior(cbs_tree_prior)) # nolint beautier function
  id <- cbs_tree_prior$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL
  text <- c(text, paste0("<distribution ",
    "id=\"BayesianSkyline.t:",
    id, "\" spec=\"BayesianSkyline\" groupSizes=\"@bGroupSizes.t:", id,
    "\" popSizes=\"@bPopSizes.t:", id, "\">"))
  text <- c(text, paste0("    ",
    "<treeIntervals id=\"BSPTreeIntervals.t:", id, "\" ",
    "spec=\"TreeIntervals\" tree=\"@Tree.t:", id, "\"/>")) # nolint this is no absolute path
  text <- c(text, paste0("</distribution>"))
  text <- c(text, paste0("<distribution id=\"MarkovChainedPopSizes.t:", id,
    "\" spec=\"beast.math.distributions.MarkovChainDistribution\" ",
    "jeffreys=\"true\" parameter=\"@bPopSizes.t:", id, "\"/>")) # nolint this is no absolute path
  text
}

#' Creates the tree prior section in the prior section of
#' the prior section of the distribution section
#' of a BEAST2 XML parameter file for a
#' Coalescent Constant Population tree prior
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
ccp_tree_prior_to_xml_prior_distr <- function( # nolint beautier function
  ccp_tree_prior
) {
  testit::assert(is_ccp_tree_prior(ccp_tree_prior)) # nolint beautier function
  id <- ccp_tree_prior$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL

  # distributions
  text <- c(text, paste0("<distribution id=\"CoalescentConstant.t:", id,
    "\" spec=\"Coalescent\">"))
  text <- c(text, paste0("    ",
    "<populationModel id=\"ConstantPopulation.t:", id,
    "\" spec=\"ConstantPopulation\" popSize=\"@popSize.t:", id, "\"/>")) # nolint this is no absolute path
  text <- c(text, paste0(
    "    <treeIntervals id=\"TreeIntervals.t:",
    id, "\" spec=\"TreeIntervals\" tree=\"@Tree.t:",
    id, "\"/>")) # nolint this is no absolute path
  text <- c(text, "</distribution>")

  # pop size
  text <- c(text, paste0(
    "<prior id=\"PopSizePrior.t:", id,
    "\" name=\"distribution\" x=\"@popSize.t:",
    id, "\">"))
  text <- c(text,
    indent( # nolint beautier function
      distr_to_xml( # nolint beautier function
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
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
cep_tree_prior_to_xml_prior_distr <- function( # nolint beautier function
  cep_tree_prior
) {
  testit::assert(is_cep_tree_prior(cep_tree_prior)) # nolint beautier function
  id <- cep_tree_prior$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL

  # distribution
  text <- c(text, paste0("<distribution ",
    "id=\"CoalescentExponential.t:", id, "\" spec=\"Coalescent\">"))
  text <- c(text, paste0("    <populationModel ",
    "id=\"ExponentialGrowth.t:", id, "\" spec=\"ExponentialGrowth\" ",
    "growthRate=\"@growthRate.t:", id, "\" ",
    "popSize=\"@ePopSize.t:", id, "\"/>")) # nolint this is no absolute path
  text <- c(text, paste0("    <treeIntervals ",
    "id=\"TreeIntervals.t:", id, "\" spec=\"TreeIntervals\" ",
    "tree=\"@Tree.t:", id, "\"/>")) # nolint this is no absolute path
  text <- c(text, paste0("</distribution>"))

  # prior
  text <- c(text, paste0("<prior ",
    "id=\"ePopSizePrior.t:", id, "\" name=\"distribution\" ",
    "x=\"@ePopSize.t:", id, "\">"))
  text <- c(text,
    indent( # nolint beautier function
      distr_to_xml( # nolint beautier function
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
    indent( # nolint beautier function
      distr_to_xml( # nolint beautier function
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
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
yule_tree_prior_to_xml_prior_distr <- function( # nolint beautier function
  yule_tree_prior
) {
  testit::assert(is_yule_tree_prior(yule_tree_prior)) # nolint beautier function
  id <- yule_tree_prior$id
  testit::assert(is_id(id)) # nolint beautier function

  text <- NULL

  # distribution
  text <- c(text, paste0("<distribution id=\"YuleModel.t:", id,
    "\" spec=\"beast.evolution.speciation.YuleModel\" ",
    "birthDiffRate=\"@birthRate.t:", id, "\" tree=\"@Tree.t:", id, "\"/>")) # nolint this is no absolute path

  # prior
  text <- c(text, paste0(
      "<prior id=\"YuleBirthRatePrior.t:", id, "\" ",
      "name=\"distribution\" x=\"@birthRate.t:", id, "\">"
    )
  )
  text <- c(text,
    indent( # nolint beautier function
      distr_to_xml(yule_tree_prior$birth_rate_distr), # nolint beautier function
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
