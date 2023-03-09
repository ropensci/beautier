test_that("use", {
  expect_silent(
    gamma_site_model_to_xml_prior_distr(
      site_model = create_jc69_site_model(id = 1),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_silent(
    gamma_site_model_to_xml_prior_distr(
      site_model = create_jc69_site_model(id = 1),
      beauti_options = create_beauti_options_v2_6()
    )
  )

  expect_silent(
    gamma_site_model_to_xml_prior_distr(
      site_model = create_hky_site_model(
        id = 1,
        kappa_prior_distr = create_uniform_distr(id = 2)
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  expect_silent(
    gamma_site_model_to_xml_prior_distr(
      site_model = create_hky_site_model(
        id = 1,
        kappa_prior_distr = create_uniform_distr(id = 2)
      ),
      beauti_options = create_beauti_options_v2_6()
    )
  )
})

test_that("use", {
  skip("WIP #135")
  # v2.6 has a FrequenciesPrior section (see below), where v2.4 does not.
  #
  # <prior id=\"FrequenciesPrior.s:anthus_aco_sub\" name=\"distribution\" x=\"@freqParameter.s:anthus_aco_sub\">"
  #     <Uniform id=\"Uniform.3\" name=\"distr\"/>"
  # </prior>"
  #
  #
  freq_prior_regex <- "FrequenciesPrior.s"
  expect_equal(0, length(stringr::str_subset(readr::read_lines(get_beautier_path("hky_2_4.xml")), freq_prior_regex)))
  expect_equal(1, length(stringr::str_subset(readr::read_lines(get_beautier_path("hky_2_6.xml")), freq_prior_regex)))

  site_model <- create_hky_site_model(
    id = 1,
    kappa_prior_distr = create_uniform_distr(id = 2)
  )

  expect_equal(
    0,
    length(
      stringr::str_subset(
        gamma_site_model_to_xml_prior_distr(
          site_model = site_model,
          beauti_options = create_beauti_options_v2_4()
        ),
        pattern = freq_prior_regex
      )
    )
  )
  expect_equal(
    1,
    length(
      stringr::str_subset(
        gamma_site_model_to_xml_prior_distr(
          site_model = site_model,
          beauti_options = create_beauti_options_v2_6()
        ),
        pattern = freq_prior_regex
      )
    )
  )
})
