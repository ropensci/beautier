#' Is an XML opening tag with value 'section' present among the lines of
#'   the text?
#' @param lines lines of an XML text
#' @param section if NA, this function returns TRUE if there is any
#'   XML opening tag. If \code{section} is set to a certain word,
#'   this function returns TRUE if that tag matches \code{section}
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
has_xml_opening_tag <- function(
  lines,
  section = NA
) {
  if (is.null(section) ||
      (!is.character(section) && !is_one_na(section))
  ) {
    stop("'section' must be NA or a word")
  }
  if (is_one_na(section)) {
    tag <- get_xml_opening_tag(lines)
    return(!is_one_na(tag))
  }
  !is_one_na(
    find_first_xml_opening_tag_line(lines, section)
  )
}
