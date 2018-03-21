context("are_equal_xml_lines")

test_that("use", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "        <log idref=\"likelihood\"/>",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )
  lines_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "        <log idref=\"likelihood\"/>",
    "        <log idref=\"posterior\"/>",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )

  testthat::expect_true(
    beautier:::are_equal_xml_lines(lines_1, lines_1, section = "logger")
  )
  testthat::expect_false(
    beautier:::are_equal_xml_lines(lines_1, lines_2, section = "logger")
  )

})

test_that("abuse: section must be a word", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>",
    "    </logger>"
  )
  lines_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )

  testthat::expect_error(
    beautier:::are_equal_xml_lines(lines_1, lines_2, section = NA),
    "'section' must be a word"
  )

})

test_that("abuse: opening tag of lines 1 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>",
    "    </logger>"
  )

  lines_2 <- lines_1

  testthat::expect_error(
    beautier:::are_equal_xml_lines(lines_1, lines_2, section = "nonsense"),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )
})

test_that("abuse: closing tag of lines 1 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"prior\"/>",
    "    </logger_oops_incorrect_closing_tag>"
  )

  lines_2 <- lines_1

  testthat::expect_error(
    beautier:::are_equal_xml_lines(lines_1, lines_2, section = "logger"),
    "Closing tag for 'section' could not be found in 'lines_1'"
  )
})

test_that("abuse: opening tag of lines 2 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )
  lines_2 <- c(
    "    <no_logger_at_all_oops id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "    </no_logger_at_all_oops>"
  )

  testthat::expect_error(
    beautier:::are_equal_xml_lines(lines_1, lines_2, section = "logger"),
    "Opening tag for 'section' could not be found in 'lines_2'"
  )
})

test_that("abuse: closing tag of lines 2 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "        <log idref=\"likelihood\"/>",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )
  lines_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "        <log idref=\"likelihood\"/>",
    "        <log idref=\"posterior\"/>",
    "        <log idref=\"prior\"/>",
    "    </no_logger_oops>"
  )

  testthat::expect_error(
    beautier:::are_equal_xml_lines(lines_1, lines_2, section = "logger"),
    "Closing tag for 'section' could not be found in 'lines_2'"
  )
})
