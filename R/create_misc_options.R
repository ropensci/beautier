#' Function to create a misc_options, containing miscellaneous options
#'   to fine-tune the created BEAST2 XML file. Whatever option chosen
#'   here, the created XML file will be valid.
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
#' @return a misc_options
#' @author Richel J.C. Bilderbeek
#' @examples
#'   misc_options <- create_misc_options(
#'     nucleotides_uppercase = TRUE,
#'     beast2_version = "2.5"
#'   )
#'   xml <- create_beast2_input(
#'     get_fasta_filename(),
#'     misc_options = misc_options
#'   )
#'   testit::assert(is.character(xml))
#'   testit::assert(length(xml) > 1)
#' @export
create_misc_options <- function(
  capitalize_first_char_id = FALSE,
  nucleotides_uppercase = FALSE,
  beast2_version = "2.4",
  required = "",
  sequence_indent = 20
) {
  misc_options <- list(
    capitalize_first_char_id = capitalize_first_char_id,
    nucleotides_uppercase = nucleotides_uppercase,
    beast2_version = beast2_version,
    required = required,
    sequence_indent = sequence_indent
  )
  testit::assert(is_misc_options(misc_options)) # nolint internal function
  misc_options
}
