#' Create the \code{<data ..>} XML
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_data_xml <- function(
  id,
  beast2_version
) {
  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste0("id=\"", id, "\""))
  if (beast2_version == "2.6") {
    text <- c(text, "spec=\"Alignment\"")
  }
  text <- c(text, "name=\"alignment\">")
  if (beast2_version == "2.5") {
    text <- paste0(text, collapse = " ")
  }
  text
}
