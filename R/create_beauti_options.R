#' Function to create a set of `BEAUti` options.
#'
#' `BEAUti` options are settings that differ between `BEAUti`
#' version. The use of these options is mostly for testing
#' older versions
#' Whatever option chosen here, the created XML file will be valid.
#'
#' Available BEAUti options are:\cr
#'   * \link{create_beauti_options_v2_4}
#'   * \link{create_beauti_options_v2_6}
#'
#' `beautier` uses v2.4 by default, as this is when the first tests
#' were written.
#' @param capitalize_first_char_id must the ID of alignment start with a
#'   capital? TRUE if yes, FALSE if it can be left lower case (if it is
#'   lowercase)
#' @param nucleotides_uppercase must the nucleotides of the DNA sequence be
#'   in uppercase?
#' @param beast2_version the BEAST2 version
#' @param required things that may be required,
#'   for example \code{BEAST v2.5.0}
#' @param sequence_indent the number of spaces the XML \code{sequence}
#'   lines are indented
#' @param status the BEAUti status
#' @param namespace the `namespace` XML element in the `beast` XML tag.
#' @return a BEAUti options structure
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
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
create_beauti_options <- function(
  capitalize_first_char_id = FALSE,
  nucleotides_uppercase = FALSE,
  beast2_version = "2.4",
  required = "",
  sequence_indent = 20,
  status = "",
  namespace = get_default_beast_namespace_v2_4()
) {
  beauti_options <- list(
    capitalize_first_char_id = capitalize_first_char_id,
    nucleotides_uppercase = nucleotides_uppercase,
    beast2_version = beast2_version,
    required = required,
    sequence_indent = sequence_indent,
    status = status,
    namespace = namespace
  )
  check_beauti_options(beauti_options)
  beauti_options
}
