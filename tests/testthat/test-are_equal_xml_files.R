context("are_equal_xml_files")

test_that("use, one file", {

  xml <- c(
    "            <taxonset id=\"TaxonSet.anthus_aco_sub\" spec=\"TaxonSet\">",
    "                <alignment idref=\"anthus_aco_sub\"/>",
    "            </taxonset>"
  )
  filename <- "temp_are_equal_xml_files.xml"
  writeLines(xml, filename)
  testthat::expect_true(
    beautier:::are_equal_xml_files(filename, filename, section = "taxonset")
  )
  file.remove(filename)
})

test_that("use, two files", {

  xml_1 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log idref=\"posterior\"/>",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "        <log idref=\"likelihood\"/>",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )
  xml_2 <- c(
    "    <logger id=\"screenlog\" logEvery=\"1000\">",
    "        <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>",
    "        <log idref=\"likelihood\"/>",
    "        <log idref=\"posterior\"/>",
    "        <log idref=\"prior\"/>",
    "    </logger>"
  )
  filename_1 <- "temp_are_equal_xml_files_1.xml"
  filename_2 <- "temp_are_equal_xml_files_2.xml"
  writeLines(xml_1, filename_1)
  writeLines(xml_2, filename_2)

  testthat::expect_false(
    beautier:::are_equal_xml_files(
      filename_1 = filename_1,
      filename_2 = filename_2,
      section = "logger"
    )
  )
  file.remove(filename_1)
  file.remove(filename_2)
})

test_that("abuse", {

  xml <- c(
    "            <taxonset id=\"TaxonSet.anthus_aco_sub\" spec=\"TaxonSet\">",
    "                <alignment idref=\"anthus_aco_sub\"/>",
    "            </taxonset>"
  )
  filename <- "temp_are_equal_xml_files.xml"
  writeLines(xml, filename)


  testthat::expect_error(
    are_equal_xml_files(
      filename_1 = "abs.ent",
      filename_2 = filename,
      section = "a"
    ),
    "'filename_1' must be the name of a file that is present"
  )
  testthat::expect_error(
    are_equal_xml_files(
      filename_1 = filename,
      filename_2 = "abs.ent",
      section = "a"
    ),
    "'filename_2' must be the name of a file that is present"
  )
  testthat::expect_error(
    are_equal_xml_files(
      filename_1 = filename,
      filename_2 = filename,
      section = "nonsense"
    ),
    "Opening tag for 'section' could not be found in 'lines_1'"
  )

  file.remove(filename)

})
