#' Internal function
#'
#' Internal function used for debugging to
#' determine if XML files result in equivalent trees
#' @param filename_1 name of a first XML file
#' @param filename_2 name of a second XML file
#' @param section the name of the XML section, use NA to check the whole file
#' @return TRUE if the two XML files result in equivalent trees,
#'   FALSE otherwise
#' @seealso to check for equality, use \code{are_equal_xml_files}
#' @examples
#' check_empty_beautier_folder()
#'
#' are_equivalent_xml_files(
#'   filename_1 = get_beautier_path("2_4.xml"),
#'   filename_2 = get_beautier_path("2_6_0.xml")
#' )
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
are_equivalent_xml_files <- function(
  filename_1,
  filename_2,
  section = NA
) {
  beautier::check_file_exists(filename_1, "filename_1")
  beautier::check_file_exists(filename_2, "filename_2")
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
#' @author Richèl J.C. Bilderbeek
#' @export
are_equivalent_xml_lines <- function(
  lines_1,
  lines_2,
  section = NA,
  verbose = FALSE
) {
  if (beautier::is_one_na(section)) {
    return(
      beautier::are_equivalent_xml_lines_all(
        lines_1 = lines_1,
        lines_2 = lines_2,
        verbose = verbose
      )
    )
  } else {
    testit::assert(!beautier::is_one_na(section))
    return(
      beautier::are_equivalent_xml_lines_section(
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
#' @author Richèl J.C. Bilderbeek
#' @export
are_equivalent_xml_lines_all <- function(
  lines_1,
  lines_2,
  verbose = FALSE
) {
  # Remove whitespace-only lines from both files
  lines_1 <- stringr::str_subset(
    string = lines_1,
    pattern = "^[:blank:]*$",
    negate = TRUE
  )
  lines_2 <- stringr::str_subset(
    string = lines_2,
    pattern = "^[:blank:]*$",
    negate = TRUE
  )

  if (length(lines_1) != length(lines_2)) {
    if (verbose) {
      message(
        "different lengths: ",
        length(lines_1), " vs ", length(lines_2)
      )
    }
    return(FALSE)
  }
  for (line in lines_1) {
    if (!line %in% lines_2) {
      if (verbose) {
        message("line '", line, "' not found")
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
#' @author Richèl J.C. Bilderbeek
#' @export
are_equivalent_xml_lines_section <- function( # nolint don't care about internal function length
  lines_1,
  lines_2,
  section,
  verbose = FALSE
) {
  assertive::assert_is_a_string(section)
  if (section == "operators") {
    return(
      beautier::are_equivalent_xml_lines_operators(lines_1, lines_2, verbose)
    )
  }
  if (section == "loggers") {
    return(
      beautier::are_equivalent_xml_lines_loggers(lines_1, lines_2, verbose)
    )
  }
  if (!beautier::has_xml_opening_tag(lines = lines_1, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!beautier::has_xml_closing_tag(lines = lines_1, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_1', ",
      "'section' has value '", section, "'"
    )
  }
  if (!beautier::has_xml_opening_tag(lines = lines_2, section = section)) {
    stop(
      "Opening tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  if (!beautier::has_xml_closing_tag(lines = lines_2, section = section)) {
    stop(
      "Closing tag for 'section' could not be found in 'lines_2', ",
      "'section' has value '", section, "'"
    )
  }
  section_1 <- beautier::extract_xml_section_from_lines(
    lines = lines_1,
    section = section
  )
  section_2 <- beautier::extract_xml_section_from_lines(
    lines = lines_2,
    section = section
  )
  beautier::are_equivalent_xml_lines_all(
    section_1,
    section_2,
    verbose = verbose
  )
}

#' Determine if XML operator lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
are_equivalent_xml_lines_operators <- function( # nolint indeed long function name
  lines_1,
  lines_2,
  verbose = FALSE
) {
  section_1 <- beautier::extract_xml_operators_from_lines(lines_1)
  section_2 <- beautier::extract_xml_operators_from_lines(lines_2)
  beautier::are_equivalent_xml_lines_all(
    section_1, section_2, verbose = verbose
  )
}

#' Determine if XML operator lines result in equivalent trees
#' @inheritParams default_params_doc
#' @param lines_1 lines of a first XML file
#' @param lines_2 lines of a second XML file
#' @return TRUE if the two XML lines result in equivalent trees,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
are_equivalent_xml_lines_loggers <- function(# nolint don't care about internal function length
  lines_1,
  lines_2,
  verbose = FALSE
) {
  section_1 <- beautier::extract_xml_loggers_from_lines(lines_1)
  section_2 <- beautier::extract_xml_loggers_from_lines(lines_2)
  beautier::are_equivalent_xml_lines_all(
    section_1,
    section_2,
    verbose = verbose
  )
}
