#' Creates the distribution section of a BEAST2 XML parameter file.
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @seealso \code{\link{create_beast2_input}}
#' @examples
#' check_empty_beautier_folder()
#'
#' inference_model <- init_inference_model(
#'   input_filename = get_fasta_filename(),
#'   inference_model = create_inference_model(
#'     beauti_options = create_beauti_options_v2_4()
#'   )
#' )
#' create_beast2_input_distr(
#'   inference_model = inference_model
#' )
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_distr <- function(
  inference_model
) {

  text <- NULL

  # prior
  text <- c(
    text,
    create_beast2_input_distr_prior(
      inference_model = inference_model
    )
  )

  # likelihood
  text <- c(
    text,
    create_beast2_input_distr_lh(
      inference_model = inference_model
    )
  )
  text <- beautier::indent(text)
  text <- c(
    "<distribution id=\"posterior\" spec=\"util.CompoundDistribution\">",
    text
  )
  text <- c(text, "</distribution>") # posterior distribution
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
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
create_beast2_input_distr_prior <- function( # nolint indeed long function name
  inference_model
) {

  # Do not be smart yet
  site_models <- list(inference_model$site_model)
  tree_priors <- list(inference_model$tree_prior)

  text <- NULL
  text <- c(text, beautier::tree_priors_to_xml_prior_distr(tree_priors))
  text <- c(text, beautier::gamma_site_models_to_xml_prior_distr(site_models))
  text <- c(text, beautier::site_models_to_xml_prior_distr(site_models))
  text <- c(
    text,
    beautier::mrca_priors_to_xml_prior_distr(inference_model = inference_model)
  )
  text <- c(
    text,
    beautier::clock_model_to_xml_prior_distr(
      inference_mode = inference_model
    )
  )

  text <- beautier::indent(text)

  # Surround text by prior distribution tag
  text <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">",
    text)
  text <- c(text, "</distribution>")
}



#' Creates the XML text for the \code{distribution} tag
#' with the \code{likelihood} ID,
#' of a BEAST2 parameter file.
#'
#' Creates the XML text for the \code{distribution} tag
#' with the \code{likelihood} ID,
#' of a BEAST2 parameter file,
#' in an unindented form
#'
#' The \code{distribution} tag (with ID equals \code{likelihood})
#' has these elements:
#'
#' \preformatted{
#'   <distribution id="likelihood"[...]>
#'      <distribution id="treeLikelihood"[...]>
#'        [...]
#'      </distribution>
#'   </distribution>
#' }
#'
#' The \code{distribution} section with ID \code{treeLikelihood}
#' is created by \link{create_tree_likelihood_distr_xml}.
#'
#' Zooming out:
#'
#' \preformatted{
#'   <beast[...]>
#'     <run[...]>
#'       <distribution id="posterior"[...]>
#'         <distribution id="likelihood"[...]>
#'           [this section]
#'         </distribution>
#'       </distribution>
#'     </run>
#'   </beast>
#' }
#'
#' @inheritParams default_params_doc
#' @note this function is not intended for regular use, thus its
#'   long name length is accepted
#' @author Richèl J.C. Bilderbeek
#' @seealso this function is called by \code{create_beast2_input_distr},
#'   together with \code{create_beast2_input_distr_prior}
#' @export
create_beast2_input_distr_lh <- function(
  inference_model
) {
  text <- beautier::create_tree_likelihood_distr_xml(inference_model)

  text <- beautier::indent(text)

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
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
bd_tree_prior_to_xml_prior_distr <- function( # nolint indeed long function name
  bd_tree_prior
) {
  testit::assert(beautier::is_bd_tree_prior(bd_tree_prior))
  id <- bd_tree_prior$id
  testit::assert(beautier::is_id(id))

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
    beautier::indent(
      beautier::distr_to_xml(
        distr = bd_birth_rate_distr
      )
    )
  )
  text <- c(text, paste0("</prior>"))

  # BDDeathRate
  bd_death_rate_distr <- bd_tree_prior$death_rate_distr

  text <- c(text, paste0("<prior id=\"DeathRatePrior.t:", id,
    "\" name=\"distribution\" x=\"@BDDeathRate.t:", id, "\">"))
  text <- c(text,
    beautier::indent(
      beautier::distr_to_xml(
        distr = bd_death_rate_distr
      )
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
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
cbs_tree_prior_to_xml_prior_distr <- function( # nolint indeed long function name
  cbs_tree_prior
) {
  testit::assert(beautier::is_cbs_tree_prior(cbs_tree_prior))
  id <- cbs_tree_prior$id
  testit::assert(beautier::is_id(id))

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
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
ccp_tree_prior_to_xml_prior_distr <- function( # nolint indeed long function name
  ccp_tree_prior
) {
  testit::assert(beautier::is_ccp_tree_prior(ccp_tree_prior))
  id <- ccp_tree_prior$id
  testit::assert(beautier::is_id(id))

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
    beautier::indent(
      beautier::distr_to_xml(
        distr = ccp_tree_prior$pop_size_distr
      )
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
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
cep_tree_prior_to_xml_prior_distr <- function( # nolint indeed long function name
  cep_tree_prior
) {
  testit::assert(beautier::is_cep_tree_prior(cep_tree_prior))
  id <- cep_tree_prior$id
  testit::assert(beautier::is_id(id))

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
    beautier::indent(
      beautier::distr_to_xml(
        distr = cep_tree_prior$pop_size_distr
      )
    )
  )
  text <- c(text, paste0("</prior>"))

  text <- c(text, paste0("<prior ",
    "id=\"GrowthRatePrior.t:", id, "\" name=\"distribution\" ",
    "x=\"@growthRate.t:", id, "\">"))
  text <- c(text,
    beautier::indent(
      beautier::distr_to_xml(
        distr = cep_tree_prior$growth_rate_distr
      )
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
#' check_empty_beautier_folder()
#'
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#'
#' check_empty_beautier_folder()
#' @export
yule_tree_prior_to_xml_prior_distr <- function( # nolint indeed long function name
  yule_tree_prior
) {
  testit::assert(beautier::is_yule_tree_prior(yule_tree_prior))
  id <- yule_tree_prior$id
  testit::assert(beautier::is_id(id))

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
    beautier::indent(
      beautier::distr_to_xml(yule_tree_prior$birth_rate_distr)
    )
  )
  text <- c(text, paste0("</prior>"))
  text
}
