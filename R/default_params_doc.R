#' Documentation of general function arguments.
#' This function does nothing.
#' It is intended to inherit function argument documentation.
#' @param alignment_id ID of the alignment,
#' as returned by \link{get_alignment_id}.
#' Keep at \code{NA} to have it initialized automatically
#' @param allow_empty_str allow a string to be empty
#' @param allow_na allow \link{NA}
#' @param alpha_parameter an alpha parameter,
#' as created by \link{create_alpha_param}
#' @param b_pop_sizes_param a Bayesian population size parameter,
#' as created by \link{create_b_pop_sizes_param}
#' @param b_pop_sizes_parameter a Bayesian population size parameter,
#' as created by \link{create_b_pop_sizes_param}
#' @param bd_tree_prior a Birth-Death tree prior, as created
#' by \code{\link{create_bd_tree_prior}}
#' @param beast2_version BEAST2 version, for example, code{"2.5"}
#' @param beauti_options one BEAUti options object,
#' as returned by \code{\link{create_beauti_options}}
#' @param beautier_folder the path to
#' the \link{beautier} temporary files folder
#' @param beta_parameter a beta parameter,
#' as created by \link{create_beta_param}
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
#' @param clock_rate_param a \code{clockRate} parameter,
#' a numeric value,
#' as created by \link{create_clock_rate_param}
#' @param crown_age the crown age of the phylogeny
#' @param crown_ages the crown ages of the phylogenies. Set to NA
#'   if the crown age needs to be estimated
#' @param distr_id a distributions' ID
#' @param fasta_filename a FASTA filename.
#' Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' Note that BEAST2 also supports missing data,
#' by using a dash (\code{-}) or question mark (\code{?})
#' as a sequence.
#' @param fasta_filenames One or more FASTA filenames.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param filename a filename, as can be checked by \link{check_filename}
#' @param fixed_crown_age determines if the phylogeny's crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param fixed_crown_ages one or more booleans to determine if the
#'   phylogenies' crown ages are fixed.
#'   If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param freq_param a `freq` parameter,
#' as created by \link{create_freq_param}
#' @param gamma_distr a gamma distribution,
#'   as created by \code{\link{create_gamma_distr}})
#' @param gamma_site_model a site model's gamma site model,
#'   as returned by \code{\link{create_gamma_site_model}}
#' @param group_sizes_dimension the group sizes' dimension,
#'   as used by the CBS tree prior (see \code{\link{create_cbs_tree_prior}})
#' @param gtr_site_model a GTR site model,
#'   as returned by \code{\link{create_gtr_site_model}}
#' @param has_non_strict_clock_model boolean to indicate that the is
#'   already at least one non-strict (i.e. relaxed log-normal) clock model
#' @param has_tip_dating TRUE if the user has supplied tip dates,
#'   FALSE otherwise
#' @param hky_site_model an HKY site model,
#'   as returned by \code{\link{create_hky_site_model}}
#' @param id an alignment's IDs.
#'   An ID can be extracted from its FASTA filename
#'   with \code{\link{get_alignment_ids_from_fasta_filenames}})
#' @param ids one or more alignments' IDs.
#'   IDs can be extracted from their FASTA filenames
#'   with \code{\link{get_alignment_ids_from_fasta_filenames}})
#' @param inference_model a Bayesian phylogenetic inference model.
#'   An inference model is the complete model setup in which a site model,
#'   clock model, tree prior and more are specified.
#'   Use \link{create_inference_model} to create an inference model.
#'   Use \link{check_inference_model} to check if  an inference model is valid.
#'   Use \link{rename_inference_model_filenames} to rename the files in an
#'   inference model.
#' @param inference_models a list of one or more inference models,
#'   as can be created by \link{create_inference_model}
#' @param initial_phylogenies one or more MCMC chain's initial phylogenies.
#'   Each one set to \code{NA} will result in BEAST2 using a random phylogeny.
#'   Else the phylogeny is assumed to be of class \code{phylo} from the
#'   \code{ape} package
#' @param input_filename A FASTA filename.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param input_filenames One or more FASTA filenames.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param is_monophyletic boolean to indicate monophyly is assumed in
#'   a Most Recent Common Ancestor prior,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param jc69_site_model a JC69 site model,
#'   as returned by \code{\link{create_jc69_site_model}}
#' @param kappa_param a kappa parameter,
#' as created by \link{create_kappa_param}
#' @param log_every number of MCMC states between writing to file
#' @param m_param an m parameter,
#' as created by \link{create_m_param}
#' @param mcmc one MCMC.
#'   Use \code{\link{create_mcmc}} to create an MCMC.
#'   Use \code{\link{create_ns_mcmc}} to create an MCMC
#'     for a Nested Sampling run.
#'   Use \code{\link{check_mcmc}} to check if an MCMC is valid.
#'   Use \code{\link{rename_mcmc_filenames}} to rename the filenames in an MCMC.
#' @param mode mode how to log.
#' Valid values are the ones returned by \link{get_log_modes}
#' @param mrca_prior a Most Recent Common Ancestor prior,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param mrca_priors a list of one or more Most Recent Common Ancestor priors,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param mrca_prior_name the unique name of the MRCA prior,
#' for example a genus, family,
#' order or even class name.
#' Leave at \link{NA} to have it named automatically.
#' @param n_init_attempts number of initialization attempts before failing
#' @param output_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @param param a parameter, as can be created by \code{\link{create_param}}.
#' @param param_id a parameter's ID
#' @param phylogeny a phylogeny of type \code{phylo} from the \code{ape}
#'   package
#' @param pop_sizes_scaler_scale_factor the scale factor used by the
#' population sizes scaler operator
#' @param pre_burnin number of burn in samples taken before entering
#'   the main loop
#' @param rate_scaler_factor the strict clock model's operator scaler
#' for the rate.
#' Use an empty string to indicate the default.
#' @param rename_fun a function to rename a filename,
#' as can be checked by \link{check_rename_fun}. This function should
#' have one argument, which will be a filename or \link{NA}. The
#' function should \link{return} one filename (when passed one filename) or
#' one \link{NA} (when passed one \link{NA}).
#' Example rename functions are:
#' \itemize{
#'   \item \link{get_remove_dir_fun} get a function that removes the directory
#'     paths from the filenames, in effect turning these into local files
#'   \item \link{get_replace_dir_fun} get a function that replaces the directory
#'     paths from the filenames
#'   \item \link{get_remove_hex_fun} get a function that removes the
#'     hex string from filenames.
#'     For example, \code{tracelog_82c1a522040.log} becomes \code{tracelog.log}
#' }
#' @param rln_clock_model a Relaxed Log-Normal clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @param sample_from_prior set to \link{TRUE} to sample from the prior
#' @param sanitise_headers set to \link{TRUE} to sanitise the headers of the
#' log file
#' @param screenlog a \code{screenlog},
#'   as created by \link{create_screenlog}
#' @param sequence_length a DNA sequence length, in base pairs
#' @param site_model a site model,
#'   as returned by \code{\link{create_site_model}}
#' @param site_model_name name of a site model,
#'   must be a name as returned by \code{\link{get_site_model_names}}
#' @param site_model_names one or more names of a site model,
#'   must be name among those returned by \code{\link{get_site_model_names}}
#' @param site_models one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @param sort how to sort the log.
#' Valid values are the ones returned by \link{get_log_sorts}
#' @param store_every number of states the MCMC will process
#'   before the posterior's state will be saved to file.
#'   Use -1 or \code{NA} to use the default frequency.
#' @param strict_clock_model a strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param taxa_names names of the taxa,
#'   as returned by \code{\link{get_taxa_names}}.
#'   Keep at \code{NA} to have it initialized automatically,
#'   using all taxa in the alignment
#' @param tipdates_filename name of the file containing the tip dates.
#'   This file is assumed to have two columns, separated by a tab.
#'   The first column contains the taxa names, the second column contains
#'   the date.
#' @param tn93_site_model a TN93 site model,
#'   as returned by \code{\link{create_tn93_site_model}}
#' @param tracelog a \code{tracelog},
#'   as created by \link{create_tracelog}
#' @param treelog a \code{treelog},
#'   as created by \link{create_treelog}
#' @param tree_prior a tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param tree_prior_name name of a tree prior,
#'   must be a name as returned by \code{\link{get_tree_prior_names}}
#' @param tree_prior_names one or more names of a tree prior,
#'   must be a name among those returned by \code{\link{get_tree_prior_names}}
#' @param tree_priors one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param verbose if TRUE, additional information is displayed, that
#'   is potentially useful in debugging
#' @param yule_tree_prior a Yule tree_prior,
#'   as created by \code{\link{create_yule_tree_prior}}
#' @author Richèl J.C. Bilderbeek
#' @note This is an internal function, so it should be marked with
#'   \code{@export}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_params_doc <- function(
  alignment_id,
  allow_empty_str,
  allow_na,
  alpha_parameter,
  b_pop_sizes_param,
  b_pop_sizes_parameter,
  bd_tree_prior,
  beautier_folder,
  cbs_tree_prior,
  beast2_version,
  beauti_options,
  beta_parameter,
  ccp_tree_prior,
  cep_tree_prior,
  chain_length,
  clock_model,
  clock_model_name,
  clock_model_names,
  clock_models,
  clock_prior_distr_id,
  clock_rate_param,
  crown_age, crown_ages,
  distr_id,
  fasta_filename, fasta_filenames,
  filename,
  fixed_crown_age,
  fixed_crown_ages,
  freq_param,
  gamma_distr,
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
  kappa_param,
  log_every,
  m_param,
  mcmc,
  mode,
  mrca_prior, mrca_priors,
  mrca_prior_name,
  n_init_attempts,
  output_filename,
  param,
  param_id,
  phylogeny,
  pop_sizes_scaler_scale_factor,
  pre_burnin,
  rate_scaler_factor,
  rename_fun,
  rln_clock_model,
  sample_from_prior,
  sanitise_headers,
  screenlog,
  sequence_length,
  site_model,
  site_model_name,
  site_model_names,
  site_models,
  sort,
  store_every,
  strict_clock_model,
  taxa_names,
  tipdates_filename,
  tn93_site_model,
  tracelog,
  treelog,
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
#' @param dimension the number of dimensions, for example, as used in
#' \link{create_freq_param}
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
#'   \code{@export}. This is not done, as this will disallow all
#'   functions to find the documentation parameters
default_parameters_doc <- function(
  dimension,
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
