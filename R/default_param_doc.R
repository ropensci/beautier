#' This function does nothing. It is intended to inherit is parameters'
#' documentation.
#' @param clock_model a clock model,
#'   as returned by \code{\link{create_clock_model}}
#' @param clock_models a list of one or more clock models,
#'   as returned by \code{\link{create_clock_model}}
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as returned by \code{\link{create_ccp_tree_prior}}
#' @param distr_id a distributions' ID
#' @param fixed_crown_age determines if the phylogeny its crown age is
#'   fixed. If FALSE, crown age is estimated by BEAST2. If TRUE,
#'   the crown age is fixed to the crown age
#'   of the initial phylogeny.
#' @param id an alignment's IDs.
#'   An ID can be extracted from its FASTA filesname
#'   with \code{\link{get_ids}})
#' @param ids one or more alignments' IDs.
#'   IDs can be extracted from their FASTA filesnames
#'   with \code{\link{get_ids}})
#' @param initial_phylogenies one or more MCMC chain's initial phylogenies.
#'   Each one set to NA will result in BEAST2 using a random phylogeny. Else
#'   the phylogeny is assumed to be of class ape::phylo.
#' @param input_fasta_filenames One or more FASTA filenames.
#'   Use \code{\link{get_fasta_filename}} to obtain a testing FASTA filename.
#' @param mcmc one mcmc object,
#'   as returned by \code{\link{create_mcmc}}
#' @param output_xml_filename Name of the XML parameter file created by this
#'   function. BEAST2 uses this file as input.
#' @param param_id a parameter's ID
#' @param misc_options one misc_options object,
#'   as returned by \code{\link{create_misc_options}}
#' @param rln_clock_model a Relaxed Log-Normal clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @param site_model a site model,
#'   as returned by \code{\link{create_site_model}}
#' @param site_models one or more site models,
#'   as returned by \code{\link{create_site_model}}
#' @param strict_clock_model a strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param tree_prior a tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @param tree_priors one or more tree priors,
#'   as returned by \code{\link{create_tree_prior}}
#' @author Richel J.C. Bilderbeek
default_params_doc <- function(
  ccp_tree_prior,
  clock_model, clock_models,
  distr_id,
  fixed_crown_age,
  initial_phylogenies,
  input_fasta_filenames,
  mcmc,
  misc_options,
  output_xml_filename,
  param_id,
  rln_clock_model,
  site_model, site_models,
  strict_clock_model,
  tree_prior, tree_priors
)
{

}
