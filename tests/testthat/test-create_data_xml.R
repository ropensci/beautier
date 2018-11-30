context("create_data_xml")

test_that("use, first data, v2.4", {
  id <- 12
  expected <- c("    <data", paste0("id=\"", id, "\""), "name=\"alignment\">")
  created <- create_data_xml(id = id, beast2_version = "2.4")
  expect_equal(expected, created)
})

test_that("use, first data, v2.5", {
  id <- 12
  expected <- paste0("    <data id=\"", id, "\" name=\"alignment\">")
  created <- create_data_xml(id = id, beast2_version = "2.5")
  expect_equal(expected, created)
})
