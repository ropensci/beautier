context("has_unique_ids")

test_that("use for unique IDs", {

  text <- c(
    parameter_to_xml(create_alpha_param(id = 0)),
    parameter_to_xml(create_alpha_param(id = 1)),
    "nothing"
  )
  testthat::expect_true(has_unique_ids(text))

})

test_that("use for dupliucate IDs", {

  text <- c(
    parameter_to_xml(create_alpha_param(id = 0)),
    parameter_to_xml(create_alpha_param(id = 0))
  )

  testthat::expect_false(has_unique_ids(text))

})
