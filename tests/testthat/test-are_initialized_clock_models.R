context("are_initialized_clock_models")

test_that("detect initialized RLN models", {

  init_rln_models <- list(
    create_rln_clock_model(
      uclstdev_distr = create_uniform_distr(id = 1)
    )
  )
  testthat::expect_true(
    beautier:::are_initialized_clock_models(init_rln_models)
  )
})


test_that("detect uninitialized rln models", {

  uninit_rln_models <- list(
    create_rln_clock_model(
      uclstdev_distr = create_uniform_distr(id = NA)
    )
  )

  testthat::expect_false(
    beautier:::are_initialized_clock_models(uninit_rln_models)
  )

})
