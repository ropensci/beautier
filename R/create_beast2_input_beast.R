#' Creates the beast section of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_beast2_input_beast <- function(
  input_filename,
  inference_model = create_inference_model()
) {
  testit::assert(length(input_filename) == 1)
  testit::assert(file.exists(input_filename))

  text <- create_beast2_beast_xml( # nolint beautier function
    beast2_version = inference_model$beauti_options$beast2_version,
    required = inference_model$beauti_options$required
  )

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_data(
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

  text <- c(text, create_beast2_input_map()) # nolint beautier function call

  text <- c(text, "")
  text <- c(text, "")

  text <- c(text,
    create_beast2_input_run(
      input_filename = input_filename,
      inference_model = inference_model
    )
  )

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
