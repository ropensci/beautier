context("get_xml_closing_tag")

test_that("use '<tag text=hello</tag>'", {

  xml <- "<parameter id=\"birthRate.t:anthus_aco_sub\" name=\"stateNode\">1.0</parameter>" # nolint XML can be long

  created <- beautier:::get_xml_closing_tag(xml)
  expected <- "parameter"
  testthat::expect_equal(created, expected)

})

test_that("use '<tag text=hello/>'", {

  xml <- "<taxon id=\"630116_aco\" spec=\"Taxon\"/>"
  created <- beautier:::get_xml_closing_tag(xml)
  testthat::expect_true(is.na(created))

})

test_that("No tag gives NA", {

  testthat::expect_true(
    is.na(
      beautier:::get_xml_closing_tag("no xml")
    )
  )
})
