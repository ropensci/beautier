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
#' Use \link{create_beast2_input_xml}
#' to create the XML text of the main XML tag.
#' Use \link{create_beast2_input_beast} to create
#' to create the XML text of the \code{beast} tag.
#' @examples
#' library(testthat)
#'
#' text <- create_beast2_input_from_model(
#'   input_filename = get_fasta_filename(),
#'   inference_model = create_inference_model()
#' )
#' expect_true(substr(text[1], 1, 5) == "<?xml")
#' expect_true(tail(text, n = 1) == "</beast>")
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_from_model <- function(
  input_filename,
  inference_model
) {
  if (length(input_filename) != 1) {
    stop("Must use one alignment, site model, clock model and tree prior")
  }
  if (!beautier::files_exist(input_filename)) {
    stop("'input_filename' not found. Value: ", input_filename)
  }
  beautier::check_inference_model(inference_model)

  inference_model <- beautier::init_inference_model(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Check if the combination of FASTA file and inference model agrees
  check_fasta_file_and_inference_model(
    input_filename = input_filename,
    inference_model = inference_model
  )

  # Make a million show as 1000000 instead of 1e+06
  old_scipen <- getOption("scipen")
  options(scipen = 20)


  text <- create_beast2_input_beast(
    input_filename = input_filename,
    inference_model = inference_model
  )
  text[1] <- paste0(create_beast2_input_xml(), text[1]) # nolint beautier function

  # Restore scipen
  options(scipen = old_scipen)

  text
}
