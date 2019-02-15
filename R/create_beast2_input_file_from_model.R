#' Create a BEAST2 input file from an inference model
#' @inheritParams default_params_doc
#' @return nothing
#' @examples
#'   # Get an example FASTA file
#'   input_filename <- get_fasta_filename()
#'
#'   # The file created by beautier, a BEAST2 input file
#'   output_filename <- tempfile()
#'
#'   create_beast2_input_file_from_model(
#'     input_filename,
#'     output_filename
#'   )
#'   testthat::expect_true(file.exists(output_filename))
#' @author Richèl J.C. Bilderbeek
#' @seealso
#'   See \code{\link{create_site_model}} for examples with
#'   different site models.
#'   See \code{\link{create_clock_model}} for examples
#'   with clock models.
#'   See \code{\link{create_tree_prior}} for examples with
#'   different tree priors.
#'   See \code{\link{create_mcmc}} for examples with
#'   a different MCMC setup.
#'   Use \link{create_beast2_input_file} to do the same with the elements
#'   of an inference model.
#' @export
create_beast2_input_file_from_model <- function( # nolint indeed a long name, but I preferred this over 'create_beast2_input_file2'
  input_filename,
  output_filename,
  inference_model = create_inference_model()
) {
  tryCatch(
    check_inference_model(inference_model), # nolint beautier function
    error = function(msg) {
      stop(
        "'inference_model' must be an inference model.\n",
        "Error: ", msg$message, "\n",
        "Value: ", inference_model
      )
    }
  )
  create_beast2_input_file( # nolint beautier function
    input_filename = input_filename,
    output_filename = output_filename,
    site_model = inference_model$site_model,
    clock_model = inference_model$clock_model,
    tree_prior = inference_model$tree_prior,
    mrca_prior = inference_model$mrca_prior,
    mcmc = inference_model$mcmc,
    beauti_options = inference_model$beauti_options,
    tipdates_filename = inference_model$tipdates_filename
  )
}
