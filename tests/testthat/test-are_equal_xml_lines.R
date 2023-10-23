context("are_equal_xml_lines")

test_that("use", {

  expect_true(
    are_equivalent_xml_lines(
      readLines(get_beautier_path("2_4.xml")),
      readLines(get_beautier_path("2_4.xml"))
    )
  )

  expect_true(
    are_equivalent_xml_lines(
      readLines(get_beautier_path("2_4.xml")),
      readLines(get_beautier_path("2_4.xml")),
      section = "operators"
    )
  )

  expect_true(
    are_equivalent_xml_lines(
      readLines(get_beautier_path("2_4.xml")),
      readLines(get_beautier_path("2_4.xml")),
      section = "loggers"
    )
  )

})

test_that("use", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>", # nolint this is no absolute path
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>", # nolint this is no absolute path
    "        <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </logger>"
  )
  lines_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>", # nolint this is no absolute path
    "        <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "        <log idref=\"posterior\"/>", # nolint this is no absolute path
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </logger>"
  )

  expect_true(
    are_equal_xml_lines(lines_1, lines_1, section = "logger")
  )
  expect_false(
    are_equal_xml_lines(lines_1, lines_2, section = "logger")
  )

})

test_that("abuse: section must be a word", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>", # nolint this is no absolute path
    "    </logger>"
  )
  lines_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </logger>"
  )

  expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = NA),
    "beautier::is_one_string"
  )

})

test_that("abuse: opening tag of lines 1 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>", # nolint this is no absolute path
    "    </logger>"
  )

  lines_2 <- lines_1

  expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = "nonsense"),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )
})

test_that("abuse: closing tag of lines 1 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </logger_oops_incorrect_closing_tag>"
  )

  lines_2 <- lines_1

  expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = "logger"),
    "Closing tag for 'section' could not be found in 'lines_1'"
  )
})

test_that("abuse: opening tag of lines 2 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </logger>"
  )
  lines_2 <- c(
    "    <no_logger_at_all_oops id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>", # nolint this is no absolute path
    "    </no_logger_at_all_oops>"
  )

  expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = "logger"),
    "Opening tag for 'section' could not be found in 'lines_2'"
  )
})

test_that("abuse: closing tag of lines 2 not found", {

  lines_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>", # nolint this is no absolute path
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>", # nolint this is no absolute path
    "        <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </logger>"
  )
  lines_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>", # nolint this is no absolute path
    "        <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "        <log idref=\"posterior\"/>", # nolint this is no absolute path
    "        <log idref=\"prior\"/>", # nolint this is no absolute path
    "    </no_logger_oops>"
  )

  expect_error(
    are_equal_xml_lines(lines_1, lines_2, section = "logger"),
    "Closing tag for 'section' could not be found in 'lines_2'"
  )
})
