#' Creates the XML text for the \code{beast} tag of a BEAST2 parameter file.
#'
#' The \code{beast} tag has these elements:
#' \preformatted{
#'   <beast[...]>
#'       <data
#'       [...]
#'       </data>
#'       [map names]
#'       <run[...]>
#'       [...]
#'       </run>
#'   </beast>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @seealso
#' Use \link{create_beast2_input_from_model} to create the complete XML text.
#' Use \link{create_beast2_input_data} to create the XML text for
#'   the \code{data} tag only.
#' Use \link{create_beast2_input_map} to create the XML text for
#'   the \code{[map names]} part.
#' Use \link{create_beast2_input_run} to create the XML text for
#'   the \code{run} tag only.
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_beast <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  testit::assert(length(input_filename) == 1)
  testit::assert(file.exists(input_filename))

  text <- beautier::create_beast2_beast_xml(
    beast2_version = inference_model$beauti_options$beast2_version,
    required = inference_model$beauti_options$required
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    beautier::create_beast2_input_data(
      input_filenames = input_filename,
      beauti_options = inference_model$beauti_options
    )
  )

  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ") # Exact same spacing as created by BEAUti
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")

  text <- c(text, beautier::create_beast2_input_map())

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    beautier::create_beast2_input_run(
      input_filename = input_filename,
      inference_model = inference_model
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
