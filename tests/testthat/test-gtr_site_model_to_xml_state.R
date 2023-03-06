test_that("equal", {
  expect_silent(
    gtr_site_model_to_xml_state(
      create_gtr_site_model(
        id = "some_id",
        rate_ct_param = create_rate_ac_param(estimate = TRUE)
      )
    )
  )
})
