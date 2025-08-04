test_that("tipdates file without tabs must give an error", {

  expect_error(
    create_inference_model(
      tipdates_filename = get_beautier_path("babette_issue_109_no_tabs.tsv")
    ),
    "is not a tab-separated file"
  )
  remove_beautier_folder()
})

test_that("produce a valid BEAST2 input file", {

  if (!"beastier" %in% installed.packages()[,1]) {
    return()
  }

  fasta_filename <- get_beautier_path("babette_issue_109.fasta")
  tipdates_filename <- get_beautier_path("babette_issue_109.tsv")
  output_filename <- get_beautier_tempfilename()

  inference_model <- create_inference_model(
    site_model = create_hky_site_model(),
    clock_model = create_rln_clock_model(),
    tree_prior = create_ccp_tree_prior(),
    mcmc = create_mcmc(),
    tipdates_filename = tipdates_filename,
    beauti_options = create_beauti_options_v2_6()
  )

  create_beast2_input_file_from_model(
    inference_model = inference_model,
    input_filename = fasta_filename,
    output_filename = output_filename
  )

  expect_true(
    is_beast2_input_file_with_tipdates(output_filename)
  )
  expect_true(
    beastier::is_beast2_input_file(output_filename)
  )
  expect_true(
    beastier::are_beast2_input_lines_deep(
      lines = readr::read_lines(output_filename)
    )
  )
  remove_beautier_folder()
})


test_that("reproduce BEAUti v2.6 file", {

  fasta_filename <- get_beautier_path("babette_issue_109.fasta")
  tipdates_filename <- get_beautier_path("babette_issue_109.tsv")
  output_filename <- get_beautier_tempfilename()
  expected_filename <- get_beautier_path("babette_issue_109_expected_v2_6.xml")

  inference_model <- create_inference_model(
    site_model = create_hky_site_model(
      kappa_prior_distr = create_log_normal_distr(
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25")
      ),
      gamma_site_model = create_gamma_site_model(
        freq_prior_uniform_distr_id = 3
      )
    ),
    clock_model = create_rln_clock_model(
      mean_rate_prior_distr = create_uniform_distr(
        id = 4
      ),
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 4, value = "0.5396"),
        beta = create_beta_param(id = 5, value = "0.3819")
      ),
      mparam_id = 3
    ),
    tree_prior = create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(
        id = 1,
        value = "0.3"
      )
    ),
    mcmc = create_mcmc(),
    tipdates_filename = tipdates_filename,
    beauti_options = create_beauti_options_v2_6(
      add_operator_schedule = FALSE
    )
  )

  create_beast2_input_file_from_model(
    inference_model = inference_model,
    input_filename = fasta_filename,
    output_filename = output_filename
  )

  expect_true(is_beast2_input_file_with_tipdates(output_filename))

  created <- readr::read_lines(output_filename)
  expected <- readr::read_lines(expected_filename)

  sum(stringr::str_detect(created, "id=\"freqParameter.s"))
  sum(stringr::str_detect(expected, "id=\"freqParameter.s"))


  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_true(are_equivalent_xml_lines(created, expected))

  remove_beautier_folder()
})
