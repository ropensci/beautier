test_that("use", {

  expect_silent(
    site_model_to_xml_state(
      create_gtr_site_model(
        id = "some_id",
        rate_ct_param = create_rate_ac_param(estimate = TRUE)
      )
    )
  )

  expect_silent(
    site_model_to_xml_state(
      create_hky_site_model(
        id = "some_id"
      )
    )
  )
  expect_silent(
    site_model_to_xml_state(
      create_jc69_site_model(
        id = "some_id"
      )
    )
  )

  expect_silent(
    site_model_to_xml_state(
      create_tn93_site_model(
        id = "some_id"
      )
    )
  )
})
