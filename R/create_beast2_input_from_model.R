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
#' text <- create_beast2_input_from_model(
#'   input_filename = get_fasta_filename(),
#'   inference_model = create_inference_model()
#' )
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
  beautier::check_inference_model(inference_model)

  inference_model <- beautier::init_inference_model(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Check if the combination of FASTA file and inference model agrees
  beautier::check_file_and_model_agree(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Make a million show as 1000000 instead of 1e+06
  old_scipen <- getOption("scipen")
  options(scipen = 20)


  text <- beautier::create_beast2_input_beast(
    input_filename = input_filename,
    inference_model = inference_model
  )
  text[1] <- paste0(
    beautier::create_xml_declaration(),
    text[1]
  )

  # Restore scipen
  options(scipen = old_scipen)

  text
}
