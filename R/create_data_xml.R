#' Create the \code{<data ..>} XML
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @noRd
create_data_xml <- function(
  id,
  beast2_version
) {
  text <- NULL
  text <- c(text, "    <data")
  text <- c(text, paste0("id=\"", id, "\""))
  text <- c(text, "name=\"alignment\">")
  if (beast2_version == "2.5") {
    text <- paste0(text, collapse = " ")
  }
  text
}
