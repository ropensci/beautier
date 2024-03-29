#' Function to create the BEAUti options for version 2.4.
#'
#' Function to create the BEAUti options for version 2.4, by
#' calling \link{create_beauti_options}.
#' @return a BEAUti options structure
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'   check_empty_beautier_folder()
#'
#'   beauti_options <- create_beauti_options_v2_4()
#'   xml <- create_beast2_input(
#'     get_fasta_filename(),
#'     beauti_options = beauti_options
#'   )
#'
#'   check_empty_beautier_folder()
#' }
#' @export
create_beauti_options_v2_4 <- function(
) {
  create_beauti_options(
    capitalize_first_char_id = FALSE,
    nucleotides_uppercase = FALSE,
    beast2_version = "2.4",
    required = "",
    sequence_indent = 20,
    namespace = get_default_beast_namespace_v2_4()
  )
}
