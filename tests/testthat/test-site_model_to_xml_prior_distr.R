test_that("minimal use, JC69", {
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_jc69_site_model(id = 1),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_jc69_site_model(id = 1),
      beauti_options = create_beauti_options_v2_6()
    )
  )
})

test_that("minimal use, HKY", {
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_hky_site_model(
        id = 1,
        kappa_prior_distr = create_uniform_distr(id = 2)
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_hky_site_model(
        id = 1,
        kappa_prior_distr = create_uniform_distr(id = 2)
      ),
      beauti_options = create_beauti_options_v2_6()
    )
  )
})

test_that("minimal use, TN93", {
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_tn93_site_model(
        id = 1,
        kappa_1_prior_distr = create_uniform_distr(id = 2),
        kappa_2_prior_distr = create_uniform_distr(id = 3)
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_tn93_site_model(
        id = 1,
        kappa_1_prior_distr = create_uniform_distr(id = 2),
        kappa_2_prior_distr = create_uniform_distr(id = 3)
      ),
      beauti_options = create_beauti_options_v2_6()
    )
  )
})

test_that("minimal use, GTR", {
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_gtr_site_model(
        id = 1,
        rate_ac_prior_distr = create_uniform_distr(id = 2),
        rate_ag_prior_distr = create_uniform_distr(id = 3),
        rate_at_prior_distr = create_uniform_distr(id = 4),
        rate_cg_prior_distr = create_uniform_distr(id = 5),
        rate_gt_prior_distr = create_uniform_distr(id = 6)
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_silent(
    site_model_to_xml_prior_distr(
      site_model = create_gtr_site_model(
        id = 1,
        rate_ac_prior_distr = create_uniform_distr(id = 2),
        rate_ag_prior_distr = create_uniform_distr(id = 3),
        rate_at_prior_distr = create_uniform_distr(id = 4),
        rate_cg_prior_distr = create_uniform_distr(id = 5),
        rate_gt_prior_distr = create_uniform_distr(id = 6)
      ),
      beauti_options = create_beauti_options_v2_6()
    )
  )
})
