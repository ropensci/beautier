context("are_init_clock_models")


test_that("detect initialized RLN clock models", {

  init_rln_models <- list(
    create_rln_clock_model(
      uclstdev_distr = create_uniform_distr(id = 1),
      m_parameter_id = 1
    )
  )
  testthat::expect_true(
    beautier:::are_init_clock_models(init_rln_models)
  )
})


test_that("detect uninitialized RLN clock models", {

  uninit_rln_models <- list(
    create_rln_clock_model(
      uclstdev_distr = create_uniform_distr(id = NA)
    )
  )

  testthat::expect_false(
    beautier:::are_init_clock_models(uninit_rln_models)
  )

})