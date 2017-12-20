#' Function to create a misc_options, containing miscellaneous options
#'   to fine-tune the created BEAST2 XML file. Whatever option chosen
#'   here, the created XML file will be valid.
#' @param capitalize_first_char_id must the ID of alignment start with a
#'   capital? TRUE if yes, FALSE if it can be left lower case (if it is
#'   lowercase)
#' @param nucleotides_uppercase must the nucleotides of the DNA sequence be
#'   in uppercase?
#' @return a misc_options
#' @author Richel J.C. Bilderbeek
#' @examples
#'   misc_options <- create_misc_options(nucleotides_uppercase = TRUE)
#'   xml <- create_beast2_input(
#'     get_fasta_filename(),
#'     misc_options = misc_options
#'   )
#'   testit::assert(is.character(xml))
#'   testit::assert(length(xml) > 1)
#' @export
create_misc_options <- function(
  capitalize_first_char_id = FALSE,
  nucleotides_uppercase = FALSE
) {
  list(
    capitalize_first_char_id = capitalize_first_char_id,
    nucleotides_uppercase = nucleotides_uppercase
  )
}
