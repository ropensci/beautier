#' Determine if XML files result in equivalent trees
#' @param filename1 name of a first XML file
#' @param filename2 name of a second XML file
#' @return TRUE if the two XML files result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
are_equivalent_xml_files <- function(
  filename1, filename2
) {
  if (!file.exists(filename1)) {
    stop("filename1 must be present")
  }
  if (!file.exists(filename2)) {
    stop("filename2 must be present")
  }
  are_equivalent_xml_lines(
    readLines(filename1),
    readLines(filename2))
}

#' Determine if XML lines result in equivalent trees
#' @param lines1 lines of a first XML file
#' @param lines2 lines of a second XML file
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
#' @export
are_equivalent_xml_lines <- function(
  lines1, lines2
) {
  if (length(lines1) != length(lines2)) {
    return(FALSE)
  }
  for (line in lines1) {
    if (!line %in% lines2) {
      return(FALSE)
    }
  }
  TRUE
}
