context("get_duplicate_ids")

test_that("use", {

  text_0 <- parameter_to_xml(create_alpha_param(id = 0))
  text_1 <- c(text_0, text_0)

  testthat::expect_equal(
    length(get_duplicate_ids(text_0)),
    0
  )

  testthat::expect_equal(
    length(get_duplicate_ids(text_1)),
    1
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_duplicate_ids(NA),
    "'text' must be text"
  )

  testthat::expect_error(
    get_duplicate_ids(NULL),
    "'text' must be text"
  )

  testthat::expect_error(
    get_duplicate_ids(ape::rcoal(3)),
    "'text' must be text"
  )

})
