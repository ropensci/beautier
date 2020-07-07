#' Function to create the BEAUti options for version 2.6.
#'
#' Function to create the BEAUti options for version 2.6, by
#' calling \link{create_beauti_options}.
#' @return a BEAUti options structure
#' @author Richèl J.C. Bilderbeek
#' @examples
#' beauti_options <- create_beauti_options_v2_6()
#' xml <- create_beast2_input(
#'   get_fasta_filename(),
#'   beauti_options = beauti_options
#' )
#'
#' library(testthat)
#' expect_true(is.character(xml))
#' expect_true(length(xml) > 1)
#' @export
create_beauti_options_v2_6 <- function(
) {
  create_beauti_options(
    beast2_version = "2.6",
    sequence_indent = 8
  )
}
