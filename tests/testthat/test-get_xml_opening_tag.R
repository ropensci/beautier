context("get_xml_opening_tag")

test_that("use '<tag text=hello</tag>'", {

  xml <- "<parameter id=\"birthRate.t:anthus_aco_sub\" name=\"stateNode\">1.0</parameter>" # nolint XML can be long

  created <- beautier:::get_xml_opening_tag(xml)
  expected <- "parameter"
  testthat::expect_equal(created, expected)

})

test_that("use '<tag text=hello/>'", {

  xml <- "<taxon id=\"630116_aco\" spec=\"Taxon\"/>"
  created <- beautier:::get_xml_opening_tag(xml)
  expected <- "taxon"
  testthat::expect_equal(created, expected)

})

test_that("No tag gives NA", {

  testthat::expect_true(
    is.na(
      beautier:::get_xml_opening_tag("no xml")
    )
  )
})
