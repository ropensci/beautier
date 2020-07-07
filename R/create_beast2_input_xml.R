#' Create the XML declaration of the BEAST2 XML input file
#' @return one line of XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' created <- create_xml_declaration()
#' expected <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
#' expect_equal(created, expected)
#' @export
create_xml_declaration <- function() {
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
}
