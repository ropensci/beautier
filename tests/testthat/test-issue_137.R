test_that("1: can re-create file 'issue_137_lognormal_estimate_beauti.xml'", {

  # Delivered by the user
  beauti_file <- beautier::get_beautier_path(
    "issue_137_lognormal_estimate_beauti.xml"
  )

  beautier_file <- get_beautier_tempfilename()
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  clock_rate <- beautier::create_clock_rate_param(
    value = "0.003536", estimate = TRUE
  )
  clock_rate_distr <- beautier::create_log_normal_distr(
    m = beautier::create_m_param(value = "-5.73"),
    s = beautier::create_s_param(value = "0.5", lower = "0.0", upper = "5.0"),
    value = "5.0"
  )
  clock_model <- beautier::create_strict_clock_model(
    id = "anthus_aco_sub",
    clock_rate_param = clock_rate,
    clock_rate_distr = clock_rate_distr
  )

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = clock_model,
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6(
      nucleotides_uppercase = TRUE,
      status = "noAutoSetClockRate"
    )
  )

  # Make the inference model match the BEAUti file
  inference_model$tree_prior$birth_rate_distr$id <- "1"
  inference_model$site_model$kappa_prior_distr$m$id <- "1"
  inference_model$site_model$kappa_prior_distr$s$id <- "2"
  inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id <- "3"
  inference_model$clock_model$clock_rate_distr$id <- "1"
  inference_model$clock_model$clock_rate_distr$m$id <- "3"
  inference_model$clock_model$clock_rate_distr$s$id <- "4"

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
  beautier::remove_beautier_folder()
})
