#' Create a BEAST2 input file
#' @inheritParams default_params_doc
#' @return nothing
#' @examples
#' # Get an example FASTA file
#' input_filename <- get_fasta_filename()
#'
#' # The file created by beautier, a BEAST2 input file
#' output_filename <- get_beautier_tempfilename()
#'
#' create_beast2_input_file(
#'   input_filename,
#'   output_filename
#' )
#' file.remove(output_filename)
#' @author Richèl J.C. Bilderbeek
#' @seealso
#'   Use \link{create_beast2_input_file_from_model} to do the same with an
#'   inference model.
#'   See \code{\link{create_site_model}} for examples with
#'   different site models. See \code{\link{create_clock_model}} for examples
#'   with clock models. See \code{\link{create_tree_prior}} for examples with
#'   different tree priors. See \code{\link{create_mcmc}} for examples with
#'   a different MCMC setup.
#' @export
create_beast2_input_file <- function(
  input_filename,
  output_filename,
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  tree_prior = beautier::create_yule_tree_prior(),
  mrca_prior = NA,
  mcmc = beautier::create_mcmc(),
  beauti_options = beautier::create_beauti_options(),
  tipdates_filename = NA
) {
  inference_model <- beautier::create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mrca_prior = mrca_prior,
    mcmc = mcmc,
    beauti_options = beauti_options,
    tipdates_filename = tipdates_filename
  )
  beautier::create_beast2_input_file_from_model(
    input_filename = input_filename,
    output_filename = output_filename,
    inference_model = inference_model
  )
  invisible()
}
