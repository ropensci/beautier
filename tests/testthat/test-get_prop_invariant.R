context("get_prop_invariant")

test_that("use", {

  testthat::expect_equal(
    get_prop_invariant(create_gamma_site_model(prop_invariant = 0.42)),
    0.42
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_prop_invariant("nonsense")
  )

})
