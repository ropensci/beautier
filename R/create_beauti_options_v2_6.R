#' Function to create the BEAUti options for version 2.6.
#'
#' Function to create the BEAUti options for version 2.6, by
#' calling \link{create_beauti_options}.
#' @inheritParams create_beauti_options
#' @return a BEAUti options structure
#' @author Richèl J.C. Bilderbeek
#' @seealso see \link{create_beauti_options_v2_4} for using the older v2.4
#' @examples
#' if (is_on_ci()) {
#'
#'   check_empty_beautier_folder()
#'
#'   beauti_options <- create_beauti_options_v2_6()
#'   xml <- create_beast2_input(
#'     get_fasta_filename(),
#'     beauti_options = beauti_options
#'   )
#'
#'   check_empty_beautier_folder()
#' }
#' @export
create_beauti_options_v2_6 <- function(
  beast2_version = "2.6",
  sequence_indent = 8,
  nucleotides_uppercase = FALSE,
  status = "",
  namespace = beautier::get_default_beast_namespace_v2_6(),
  required = ""
) {
  beautier::create_beauti_options(
    beast2_version = beast2_version,
    sequence_indent = sequence_indent,
    nucleotides_uppercase = nucleotides_uppercase,
    status = status,
    namespace = namespace,
    required = required
  )
}
