#' Create a BEAST2 XML input text
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso
#'   Use \link{create_beast2_input_from_model} to create the BEAST2 XML
#'   input text from an inference model
#'   Use \link{create_beast2_input_file} to also save it to file.
#' @examples
#'   text <- create_beast2_input(
#'     input_filename = get_fasta_filename()
#'   )
#'   testit::assert(substr(text[1], 1, 5) == "<?xml")
#'   text[1]
#'   testit::assert(tail(text, n = 1) == "</beast>")
#' @seealso \code{\link{create_beast2_input_file}} shows more examples
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input <- function(
  input_filename,
  tipdates_filename = NA,
  site_model = beautier::create_jc69_site_model(),
  clock_model = beautier::create_strict_clock_model(),
  tree_prior = beautier::create_yule_tree_prior(),
  mrca_prior = NA,
  mcmc = beautier::create_mcmc(),
  beauti_options = beautier::create_beauti_options()
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
  beautier::create_beast2_input_from_model(
    input_filename = input_filename,
    inference_model = inference_model
  )
}
