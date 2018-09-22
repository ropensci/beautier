#' Create the \code{<data ..>} XML
#' @author Richel J.C. Bilderbeek
#' @noRd
create_data_xml <- function(
  id,
  is_first,
  beast2_version
) {
  text <- NULL
  if (is_first == TRUE) {
    text <- c(text, "    <data")
  } else {
    text <- c(text, "<data")
  }
  text <- c(text, paste0("id=\"", id, "\""))
  text <- c(text, "name=\"alignment\">")
  if (beast2_version == "2.5") {
    text <- paste0(text, collapse = " ")
  }
  text
}
