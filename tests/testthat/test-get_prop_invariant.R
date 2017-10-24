context("get_prop_invariant")

test_that("use", {

  testthat::expect_equal(
    get_prop_invariant(create_hky_site_model()),
    get_default_prop_invariant()
  )

  testthat::expect_equal(
    get_prop_invariant(create_hky_site_model(prop_invariant = 0.42)),
    0.42
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_prop_invariant("nonsense")
  )

})
