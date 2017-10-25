context("get_gamma_site_model")

test_that("use", {

  testthat::expect_true(
    is_gamma_site_model(
      get_gamma_site_model(
        create_jc69_site_model()
      )
    )
  )

})

test_that("abuse", {

  testthat::expect_error(
    get_gamma_site_model("nonsense")
  )

})
