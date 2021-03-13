#' Creates the XML text for the \code{beast} tag of a BEAST2 parameter file.
#'
#' Creates the XML text for the \code{beast} tag of a BEAST2 parameter file,
#' which is directly after the XML
#' declaration (created by \link{create_xml_declaration}.
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
  inference_model = beautier::create_inference_model()
) {
  testit::assert(length(input_filename) == 1)
  testit::assert(file.exists(input_filename))

  text <- beautier::create_beast2_beast_xml(
    inference_model$beauti_options
  )

  # The first whitespace after the XML line
  if (inference_model$beauti_options$beast2_version != "2.6") {
    text <- c(text, "")
    text <- c(text, "")
  } else {
    text <- c(text, "        ")
  }

  # The <data ...> tag
  text <- c(text,
    beautier::create_beast2_input_data(
      input_filename = input_filename,
      beauti_options = inference_model$beauti_options
    )
  )

  # The second piece of whitespace after the data
  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- c(text, "        ")
  }
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  text <- c(text, "")
  text <- c(text, "    ")
  text <- c(text, "")
  if (inference_model$beauti_options$beast2_version != "2.6") {
    text <- c(text, "")
    text <- c(text, "    ")
  }

  # The [map_names] section
  xml_maps <- beautier::create_beast2_input_map(
    beauti_options = inference_model$beauti_options
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    xml_maps <- beautier::indent(xml_maps)
    xml_maps[xml_maps == ""] <- "    "
  }
  text <- c(text, xml_maps)

  # Third whitepace
  if (inference_model$beauti_options$beast2_version != "2.6") {
    text <- c(text, "")
    text <- c(text, "")
  }

  # The <run...> tag
  xml_run <- beautier::create_beast2_input_run(
    input_filename = input_filename,
    inference_model = inference_model
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    xml_run <- beautier::indent(xml_run)
    xml_run[xml_run == ""] <- "            "
  }

  text <- c(text, xml_run)

  text <- c(text, "")
  text <- c(text, "</beast>")
  text
}
