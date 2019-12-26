context("is_init_param")

test_that("use", {

  testthat::expect_false(is_init_param(create_alpha_param(id = NA)))
  testthat::expect_true(is_init_param(create_alpha_param(id = 1)))
})

test_that("abuse", {

  testthat::expect_error(
    is_init_param("nonsense"),
    "'x' must be a parameter"
  )

})
