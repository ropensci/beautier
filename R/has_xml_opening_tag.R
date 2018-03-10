#' Is an XML opening tag with value 'section' present amongst the lines of
#'   the text?
#' @param lines lines of an XML text
#' @param section if NA, this function returns TRUE if there is any
#'   XML opening tag. If \code{section} is set to a certain word,
#'   this function returns TRUE if that tag matches \code{section}
has_xml_opening_tag <- function(
  lines,
  section = NA
) {
  if (is.null(section) || (!is.character(section) && !is.na(section))) {
    stop("'section' must be NA or a word")
  }
  if (is.na(section)) {
    tag <- get_xml_opening_tag(lines) # nolint
    return(!is.na(tag))
  }
  !is.na(find_first_xml_opening_tag_line(lines, section))
}
