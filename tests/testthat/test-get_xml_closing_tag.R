context("get_xml_closing_tag")

test_that("use '<tag text=hello</tag>'", {

  xml <- "<parameter id=\"birthRate.t:anthus_aco_sub\" name=\"stateNode\">1.0</parameter>" # nolint XML can be long

  created <- get_xml_closing_tag(xml)
  expected <- "parameter"
  testthat::expect_equal(created, expected)

})

test_that("use '<tag text=hello/>'", {

  xml <- "<taxon id=\"630116_aco\" spec=\"Taxon\"/>" # nolint this is no absolute path
  created <- get_xml_closing_tag(xml)
  testthat::expect_true(is_one_na(created))

})

test_that("No tag gives NA", {

  testthat::expect_true(
    is_one_na(
      get_xml_closing_tag("no xml")
    )
  )
})
