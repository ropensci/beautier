#' Documentation of general function arguments.
#' This function does nothing.
#' It is intended to inherit function argument documentation.
#' @param alignment_id ID of the alignment,
#'   as returned by \code{\link{get_alignment_id}}.
#'   Keep at \code{NA} to have it initialized automatically
#' @param bd_tree_prior a Birth-Death tree prior, as created
#'   by \code{\link{create_bd_tree_prior}}
#' @param beauti_options one BEAUti options object,
#'   as returned by \code{\link{create_beauti_options}}
#' @param clock_prior_distr_id ID of an MRCA clock model's distribution.
#'   Keep at \code{NA} to have it initialized automatically
#' @param cbs_tree_prior a Coalescent Bayesian Skyline tree prior,
#'   as returned by \code{\link{create_cbs_tree_prior}}
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as returned by \code{\link{create_ccp_tree_prior}}
#' @param cep_tree_prior a Coalescent Exponential Population tree prior,
#'   as returned by \code{\link{create_cep_tree_prior}}
#' @param chain_length length of the MCMC chain
#' @param clock_model a clock model,
#'   as returned by \code{\link{create_clock_model}}
#' @param clock_model_name name of a clock model,
#'   must be a name as returned by \code{\link{get_clock_model_names}}
#' @param clock_model_names one or more names of a clock model,
#'   must be name among those returned by \code{\link{get_clock_model_names}}
#' @param clock_models a list of one or more clock models,
#'   as returned by \code{\link{create_clock_model}}
#' @param crown_age the crown age of the phylogeny
#' @param crown_ages the crown ages of the phylogenies. Set to NA
#'   if the crown age needs to be estimated
#' @param distr_id a distributions' ID
#' @param fasta_filename a FASTA filename.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param fasta_filenames One or more FASTA filenames.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param fixed_crown_age determines if the phylogeny's crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param fixed_crown_ages one or more booleans to determine if the
#'   phylogenies' crown ages are fixed.
#'   If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param gamma_site_model a site model's gamma site model,
#'   as returned by \code{\link{create_gamma_site_model}}
#' @param group_sizes_dimension the group sizes' dimension, as used
#'   by the CBS tree prior (see \code{\link{create_cbs_tree_prior}})
#' @param has_non_strict_clock_model boolean to indicate that the is
#'   already at least one non-strict (i.e. relaxed log-normal) clock model
#' @param has_tip_dating TRUE if the user has supplied tip dates,
#'   FALSE otherwise
#' @param hky_site_model an HKY site model,
#'   as returned by \code{\link{create_hky_site_model}}
#' @param is_monophyletic boolean to indicate monophyly is assumed in
#'   a Most Recent Common Ancestor prior,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param jc69_site_model a JC69 site model,
#'   as returned by \code{\link{create_jc69_site_model}}
#' @param gtr_site_model a GTR site model,
#'   as returned by \code{\link{create_gtr_site_model}}
#' @param id an alignment's IDs.
#'   An ID can be extracted from its FASTA filename
#'   with \code{\link{get_alignment_ids}})
#' @param ids one or more alignments' IDs.
#'   IDs can be extracted from their FASTA filenames
#'   with \code{\link{get_alignment_ids}})
#' @param inference_model an Bayesian phylogenetic inference model,
#'   as can be created by \code{\link{create_inference_model}}.
#'   An inference model is the complete model setup in which a site model,
#'   clock model, tree prior and more are specified.
#' @param inference_models a list of one or more inference models,
#'   as can be created by \link{create_inference_model}
#' @param initial_phylogenies one or more MCMC chain's initial phylogenies.
#'   Each one set to \code{NA} will result in BEAST2 using a random phylogeny.
#'   Else the phylogeny is assumed to be of class \link[ape]{phylo}
#' @param input_filename A FASTA filename.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param input_filenames One or more FASTA filenames.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param mcmc one MCMC
#'   as returned by \code{\link{create_mcmc}}
#' @param mrca_prior a Most Recent Common Ancestor prior,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param mrca_priors a list of one or more Most Recent Common Ancestor priors,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param output_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @param param_id a parameter's ID
#' @param phylogeny a phylogeny of type \code{\link[ape]{phylo}}
#' @param posterior_crown_age deprecated
#' @param rln_clock_model a Relaxed Log-Normal clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @param sequence_length a DNA sequence length, in base pairs
#' @param site_model a site model,
#'   as returned by \code{\link{create_site_model}}
#' @param site_model_name name of a site model,
#'   must be a name as returned by \code{\link{get_site_model_names}}
#' @param site_model_names one or more names of a site model,
#'   must be name among those returned by \code{\link{get_site_model_names}}
#' @param site_models one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @param store_every number of states the MCMC will process
#'   before the posterior's state will be saved to file.
#'   Use -1 or \code{NA} to use the default frequency.
#' @param strict_clock_model a strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param tn93_site_model a TN93 site model,
#'   as returned by \code{\link{create_tn93_site_model}}
#' @param tree_prior a tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param tree_prior_name name of a tree prior,
#'   must be a name as returned by \code{\link{get_tree_prior_names}}
#' @param tree_prior_names one or more names of a tree prior,
#'   must be a name among those returned by \code{\link{get_tree_prior_names}}
#' @param tree_priors one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param tipdates_filename name of the file containing the tip dates.
#'   This file is assumed to have two columns, separated by a tab.
#'   The first column contains the taxa names, the second column contains
#'   the date.
#' @param verbose if TRUE, additional information is displayed, that
#'   is potentially useful in debugging
#' @param yule_tree_prior a Yule tree_prior,
#'   as created by \code{\link{create_yule_tree_prior}}
#' @author Richèl J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_params_doc <- function(
  alignment_id,
  bd_tree_prior,
  cbs_tree_prior,
  beauti_options,
  ccp_tree_prior,
  cep_tree_prior,
  chain_length,
  clock_model,
  clock_model_name,
  clock_model_names,
  clock_models,
  clock_prior_distr_id,
  crown_age, crown_ages,
  distr_id,
  fasta_filename, fasta_filenames,
  fixed_crown_age,
  fixed_crown_ages,
  gamma_site_model,
  group_sizes_dimension,
  gtr_site_model,
  has_non_strict_clock_model,
  has_tip_dating,
  hky_site_model,
  id,
  ids,
  inference_model, inference_models,
  initial_phylogenies,
  input_filename, input_filenames,
  is_monophyletic,
  jc69_site_model,
  mcmc,
  mrca_prior, mrca_priors,
  output_filename,
  param_id,
  phylogeny,
  posterior_crown_age,
  rln_clock_model,
  sequence_length,
  site_model,
  site_model_name,
  site_model_names,
  site_models,
  store_every,
  strict_clock_model,
  tipdates_filename,
  tn93_site_model,
  tree_prior,
  tree_prior_name,
  tree_prior_names,
  tree_priors,
  verbose,
  yule_tree_prior
) {
  # Nothing
}

#' Documentation of parameters (for example, \code{create_param}.
#' This function does nothing. It is intended to inherit documentation from.
#' @param estimate TRUE if this parameter is to be estimated by BEAST2,
#'   FALSE otherwise
#' @param id the parameter's ID
#' @param lower lowest possible value of the parameter. If the parameter
#'   is estimated, \code{lower} must be less than \code{value}
#' @param name the parameters' name. Valid
#'   names can be found in \code{get_param_names}
#' @param upper upper value of the parameter
#' @param value value of the parameter
#' @param ... specific parameter parameters
#' @author Richèl J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@noRd}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_parameters_doc <- function(
  estimate,
  id,
  lower,
  name,
  upper,
  value,
  ...
) {
  # Nothing
}
