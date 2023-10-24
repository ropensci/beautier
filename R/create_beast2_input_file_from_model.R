#' Create a BEAST2 input file from an inference model
#' @inheritParams default_params_doc
#' @return nothing
#' @seealso use \link{create_beast2_input_from_model} to
#' get the BEAST2 input file as text
#' @examples
#' if (is_on_ci()) {
#'   check_empty_beautier_folder()
#'
#'   output_filename <- get_beautier_tempfilename()
#'   create_beast2_input_file_from_model(
#'     input_filename = get_fasta_filename(),
#'     output_filename = output_filename,
#'     inference_model = create_inference_model()
#'   )
#'   file.remove(output_filename)
#'
#'   remove_beautier_folder()
#'   check_empty_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @seealso
#' See \code{\link{create_site_model}} for examples with
#' different site models.
#' See \code{\link{create_clock_model}} for examples
#' with clock models.
#' See \code{\link{create_tree_prior}} for examples with
#' different tree priors.
#' See \code{\link{create_mcmc}} for examples with
#' a different MCMC setup.
#' Use \link{create_beast2_input_file} to do the same with the elements
#' of an inference model.
#' @export
create_beast2_input_file_from_model <- function( # nolint indeed a long name, but I preferred this over 'create_beast2_input_file2'
  input_filename,
  output_filename,
  inference_model = create_inference_model()
) {
  tryCatch(
    check_inference_model(inference_model),
    error = function(msg) {
      stop(
        "'inference_model' must be an inference model.\n",
        "Error: ", msg$message, "\n",
        "Value: ", inference_model
      )
    }
  )
  text <- create_beast2_input_from_model(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Create sub-sub-sub-foler if needed
  dir.create(dirname(output_filename), showWarnings = FALSE, recursive = TRUE)

  # Write to file
  tryCatch(
    suppressWarnings(
      writeLines(text, con = output_filename)
    ),
    error = function(e) {
      stop(
        "Cannot write to file with name '", output_filename, "' \n",
        "Perhaps no permission to write there? \n",
        "Error message: ", e$message, " \n"
      )
    }
  )
  invisible()
}
