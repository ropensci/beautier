#' Create a BEAST2 XML input text from an inference model
#'
#' The main two XML tags are these:
#' \preformatted{
#'   <?xml[...]?><beast[...]>
#'   [...]
#'   </beast>
#' }
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @seealso
#' Use \link{create_beast2_input_file_from_model} to also save it to file.
#' Use \link{create_xml_declaration}
#' to create the XML text of the XML declaration.
#' Use \link{create_beast2_input_beast} to create
#' to create the XML text of the \code{beast} tag.
#' @examples
#' if (is_on_ci()) {
#'
#'   check_empty_beautier_folder()
#'
#'   text <- create_beast2_input_from_model(
#'     input_filename = get_fasta_filename(),
#'     inference_model = create_inference_model()
#'   )
#'
#'   check_empty_beautier_folder()
#' }
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_from_model <- function(
  input_filename,
  inference_model
) {
  if (length(input_filename) != 1) {
    stop("Must use one alignment, site model, clock model and tree prior")
  }
  if (!file.exists(input_filename)) {
    stop("'input_filename' not found. Value: ", input_filename)
  }
  check_inference_model(inference_model)

  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Check if the combination of FASTA file and inference model agrees
  check_file_and_model_agree(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Make a million show as 1000000 instead of 1e+06
  # Use suprior syntax, thanks Victoria Wimmer
  old <- options()
  on.exit(options(old))
  options(scipen = 20)

  text <- create_beast2_input_beast(
    input_filename = input_filename,
    inference_model = inference_model
  )
  text[1] <- paste0(
    create_xml_declaration(),
    text[1]
  )

  text
}
