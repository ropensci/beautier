context("site_model_to_xml_state")

test_that("use", {

  testthat::expect_silent(
    beautier:::site_model_to_xml_state(
      create_gtr_site_model(
        id = "some_id",
        rate_ct_param = create_rate_ac_param(estimate = TRUE)
      )
    )
  )
})
