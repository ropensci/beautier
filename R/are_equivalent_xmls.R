#' Determine if XML files result in equivalent trees
#' @param filename_1 name of a first XML file
#' @param filename_2 name of a second XML file
#' @param section the name of the XML section, use NA to check the whole file
#' @return TRUE if the two XML files result in equivalent trees,
#'   FALSE otherwise
#' @seealso to check for equality, use \code{\link{are_equal_xml_files}}
#' @author Richel J.C. Bilderbeek
are_equivalent_xml_files <- function(
  filename_1,
  filename_2,
  section = NA
) {
  if (!file.exists(filename_1)) {
    stop(
      "'filename_1' must be the name of a present file. ",
      "File name '", filename_1, "' not found"
    )
  }
  if (!file.exists(filename_2)) {
    stop(
      "'filename_2' must be the name of a present file. ",
      "File name '", filename_2, "' not found"
    )
  }
  are_equivalent_xml_lines(
    readLines(filename_1),
    readLines(filename_2),
    section = section
  )
}

#' Determine if XML lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @param section the name of the XML section
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
are_equivalent_xml_lines <- function(
  lines_1,
  lines_2,
  section = NA,
  verbose = FALSE
) {
  if (is.na(section)) {
    return(
      are_equivalent_xml_lines_all(
        lines_1 = lines_1,
        lines_2 = lines_2,
        verbose = verbose
      )
    )
  } else {
    testit::assert(!is.na(section))
    return(
      are_equivalent_xml_lines_section(
        lines_1 = lines_1,
        lines_2 = lines_2,
        section = section,
        verbose = verbose
      )
    )
  }
}

#' Determine if XML lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
are_equivalent_xml_lines_all <- function(
  lines_1,
  lines_2,
  verbose = FALSE
) {
  if (length(lines_1) != length(lines_2)) {
    if (verbose) {
      print(paste0("different lengths: ",
        length(lines_1), " vs ", length(lines_2)))
    }
    return(FALSE)
  }
  for (line in lines_1) {
    if (!line %in% lines_2) {
      if (verbose) {
        print(paste0("line '", line, "' not found"))
      }
      return(FALSE)
    }
  }
  TRUE
}

#' Determine if XML lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @param section the name of the XML section
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
are_equivalent_xml_lines_section <- function( # nolint don't care about internal function length
  lines_1,
  lines_2,
  section,
  verbose = FALSE
) {
  if (!is.character(section)) {
    stop("'section' must be a word")
  }
  if (section == "operators") {
    return(are_equivalent_xml_lines_operators(lines_1, lines_2, verbose)) # nolint internal function
  }
  if (section == "loggers") {
    return(are_equivalent_xml_lines_loggers(lines_1, lines_2, verbose)) # nolint internal function
  }
  if (!has_xml_opening_tag(lines = lines_1, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_closing_tag(lines = lines_1, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_opening_tag(lines = lines_2, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  if (!has_xml_closing_tag(lines = lines_2, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  section_1 <- extract_xml_section_from_lines(
    lines = lines_1, section = section)
  section_2 <- extract_xml_section_from_lines(
    lines = lines_2, section = section)
  are_equivalent_xml_lines_all(section_1, section_2, verbose = verbose) # nolint internal function
}

#' Determine if XML operator lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
are_equivalent_xml_lines_operators <- function( # nolint don't care about internal function length
  lines_1,
  lines_2,
  verbose = FALSE
) {
  section_1 <- extract_xml_operators_from_lines(lines_1) # nolint internal function
  section_2 <- extract_xml_operators_from_lines(lines_2) # nolint internal function
  are_equivalent_xml_lines_all(section_1, section_2, verbose = verbose) # nolint internal function
}

#' Determine if XML operator lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richel J.C. Bilderbeek
are_equivalent_xml_lines_loggers <- function( # nolint don't care about internal function length
  lines_1,
  lines_2,
  verbose = FALSE
) {
  section_1 <- extract_xml_loggers_from_lines(lines_1) # nolint internal function
  section_2 <- extract_xml_loggers_from_lines(lines_2) # nolint internal function
  are_equivalent_xml_lines_all(section_1, section_2, verbose = verbose) # nolint internal function
}
