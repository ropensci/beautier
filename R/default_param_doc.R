#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param alignment_id ID of the alignment,
#'   as returned by \code{\link{get_alignment_id}}
#' @param bd_tree_prior a Birth-Death tree prior, as created
#'   by \code{\link{create_bd_tree_prior}}
#' @param clock_prior_distr_id ID of an MRCA clock model's distribution.
#'   Keep at NA to have it initialized automatically
#' @param cbs_tree_prior a Coalescent Bayesian Skyline tree prior,
#'   as returned by \code{\link{create_cbs_tree_prior}}
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as returned by \code{\link{create_ccp_tree_prior}}
#' @param cep_tree_prior a cep tree prior,
#'   as returned by \code{\link{create_cep_tree_prior}}
#' @param clock_model a clock model,
#'   as returned by \code{\link{create_clock_model}}
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
#'   An ID can be extracted from its FASTA filesname
#'   with \code{\link{get_ids}})
#' @param ids one or more alignments' IDs.
#'   IDs can be extracted from their FASTA filesnames
#'   with \code{\link{get_ids}})
#' @param initial_phylogenies one or more MCMC chain's initial phylogenies.
#'   Each one set to NA will result in BEAST2 using a random phylogeny. Else
#'   the phylogeny is assumed to be of class ape::phylo.
#' @param input_filenames One or more FASTA filenames.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param is_first boolean to indicate if this is the first
#'   out of many (one, two or more)
#' @param mcmc one mcmc object,
#'   as returned by \code{\link{create_mcmc}}
#' @param misc_options one misc_options object,
#'   as returned by \code{\link{create_misc_options}}
#' @param mrca_prior a Most Recent Common Ancestor prior,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param mrca_priors a list of one or more Most Recent Common Ancestor priors,
#'   as returned by \code{\link{create_mrca_prior}}
#' @param output_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @param param_id a parameter's ID
#' @param posterior_crown_age the crown age the posteriors'
#'   phylogenies will be fixed at. If NA, crown age is estimated by BEAST2.
#' @param rln_clock_model a Relaxed Log-Normal clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @param sequence_length a DNA sequence length, in base pairs
#' @param site_model a site model,
#'   as returned by \code{\link{create_site_model}}
#' @param site_models one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @param strict_clock_model a strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param tn93_site_model a TN93 site model,
#'   as returned by \code{\link{create_tn93_site_model}}
#' @param tree_prior a tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param tree_priors one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param verbose if TRUE, additional information is displayed, that
#'   is potentially useful in debugging
#' @param yule_tree_prior a Yule tree_prior,
#'   as created by \code{\link{create_yule_tree_prior}}
#' @author Richel J.C. Bilderbeek
default_params_doc <- function(
  alignment_id,
  bd_tree_prior,
  cbs_tree_prior,
  ccp_tree_prior,
  cep_tree_prior,
  clock_model, clock_models,
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
  hky_site_model,
  id, ids,
  initial_phylogenies,
  input_filenames,
  is_first,
  is_monophyletic,
  jc69_site_model,
  mcmc,
  misc_options,
  mrca_prior, mrca_priors,
  output_filename,
  param_id,
  posterior_crown_age,
  rln_clock_model,
  sequence_length,
  site_model, site_models,
  strict_clock_model,
  tn93_site_model,
  tree_prior, tree_priors,
  verbose,
  yule_tree_prior
) {
  # Nothing
}
