context("create_random_name")

test_that("use", {

  testthat::expect_silent(
    beautier:::create_random_name()
  )
})

test_that("should follow RNG", {

  set.seed(42)
  name_1 <- beautier:::create_random_name()
  set.seed(42)
  name_2 <- beautier:::create_random_name()
  name_3 <- beautier:::create_random_name()
  testthat::expect_true(name_1 == name_2)
  testthat::expect_true(name_1 != name_3)
})
