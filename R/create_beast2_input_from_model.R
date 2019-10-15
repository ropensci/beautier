#' Create a BEAST2 XML input text from an inference model
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso
#'   Use \link{create_beast2_input_file_from_model} to also save it to file.
#' @examples
#'   text <- create_beast2_input_from_model(
#'     input_filename = get_fasta_filename()
#'   )
#'   testit::assert(substr(text[1], 1, 5) == "<?xml")
#'   text[1]
#'   testit::assert(tail(text, n = 1) == "</beast>")
#' @seealso \code{\link{create_beast2_input_file}} shows more examples
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_from_model <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  create_beast2_input(
    input_filename = input_filename,
    tipdates_filename = inference_model$tipdates_filename,
    site_model = inference_model$site_model,
    clock_model = inference_model$clock_model,
    tree_prior = inference_model$tree_prior,
    mrca_prior = inference_model$mrca_prior,
    mcmc = inference_model$mcmc,
    beauti_options = inference_model$beauti_options
  )
}

