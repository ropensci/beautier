#' Determine if XML files result in equal trees
#' @param filename_1 name of a first XML file
#' @param filename_2 name of a second XML file
#' @param section name of an XML section.
#'   Assumes that there is one line that starts with \code{<section}
#'   (excluding whitespace)
#'   and one line that is \code{</section>} (also excluding whitespace)
#' @return TRUE if the two sections of the XML files are equal,
#'   FALSE otherwise
#' @seealso to check for equivalence, use \code{\link{are_equivalent_xml_files}}
#' @author Richel J.C. Bilderbeek
are_equal_xml_files <- function(
  filename_1,
  filename_2,
  section
) {
  if (!file.exists(filename_1)) {
    stop(
      "'filename_1' must be the name of a file that is present, ",
      "'", filename_1, "' could not be found"
    )
  }
  if (!file.exists(filename_2)) {
    stop(
      "'filename_2' must be the name of a file that is present, ",
      "'", filename_2, "' could not be found"
    )
  }
  are_equal_xml_lines(
    lines_1 = readLines(filename_1),
    lines_2 = readLines(filename_2),
    section = section
  )
}
