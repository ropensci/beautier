test_that("1: can re-create file 'issue_135_no_mrca_no_estimate_beauti.xml'", {
  beauti_file <- beautier::get_beautier_path("issue_135_no_mrca_no_estimate_beauti.xml")

  beautier_file <- get_beautier_tempfilename()

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")
  clock.rate <- beautier::create_clock_rate_param(value = 0.00277, estimate=FALSE)

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA, clock.rate),
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6(nucleotides_uppercase = TRUE)
  )

  inference_model$tree_prior$birth_rate_distr$id <- "1"
  inference_model$site_model$kappa_prior_distr$m$id <- "1"
  inference_model$site_model$kappa_prior_distr$s$id <- "2"
  inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id <- "3"
  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))

  beauti_text <- readr::read_lines(beauti_file)
  beautier_text <- readr::read_lines(beautier_file)
  if (1 == 2) {
    beautier::compare_lines(
      lines = beautier_text,
      expected = beauti_text,
      created_lines_filename = "~/created.xml",
      expected_lines_filename = "~/expected.xml"
    )
  }
  beautier::remove_beautier_folder()
})

test_that("2: can re-create file 'issue_135_no_mrca_estimate_beauti.xml'", {

  # Delivered by the user
  beauti_file <- beautier::get_beautier_path("issue_135_no_mrca_estimate_beauti.xml")

  beautier_file <- get_beautier_tempfilename()

  #134 without mrca prior, estimating clock rate from a uniform prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  clock.rate <- beautier::create_clock_rate_param(value = "0.0035", estimate = TRUE)
  clock.uniform <- beautier::create_uniform_distr(value = 0.0035, lower = 0.00277, upper = 0.00542)

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(
      id = NA,
      clock_rate_param = clock.rate,
      clock_rate_distr = clock.uniform
    ),
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
  inference_model$clock_model$clock_rate_distr$id <- "0"

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))

  beautier::remove_beautier_folder()
})


test_that("3: can re-create file 'issue_135_mrca_no_estimate_beauti.xml'", {

  # This is a unique file, delivered by the user
  beauti_file <- beautier::get_beautier_path("issue_135_mrca_no_estimate_beauti.xml")
  beautier_file <- get_beautier_tempfilename()

  #134 without mrca prior, estimating clock rate from a uniform prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  #With mrca prior, single value at clock rate
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca.taxa <- get_taxa_names(fasta_filename)
  mrca.taxa <- mrca.taxa[2:length(mrca.taxa)]
  mrca.prior <- create_mrca_prior(taxa_names=mrca.taxa,is_monophyletic = TRUE)
  clock.rate <- beautier::create_clock_rate_param(value = 0.00277,estimate=FALSE)

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA,clock.rate),
    tree_prior = create_yule_tree_prior(),
    mrca_prior = mrca.prior,
    beauti_options = beautier::create_beauti_options_v2_6(
      nucleotides_uppercase = TRUE
    )
  )

  # Make the inference model match the BEAUti file
  inference_model$tree_prior$birth_rate_distr$id <- "1"
  inference_model$site_model$kappa_prior_distr$m$id <- "1"
  inference_model$site_model$kappa_prior_distr$s$id <- "2"
  inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id <- "3"
  inference_model$clock_model$clock_rate_distr$id <- "0"

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
})


test_that("4: can re-create file 'issue_135_mrca_estimate_beauti.xml'", {
  skip("Issue #135, Issue 135")

  # This is a unique file, delivered by the user
  beauti_file <- beautier::get_beautier_path("issue_135_mrca_estimate_beauti.xml")

  file.copy(beauti_file, "~/issue_135_mrca_estimate_beauti.xml")
  beautier_file <- "~/issue_135_mrca_estimate_beautier.xml"
  # beautier_file <- get_beautier_tempfilename()

  #With mrca prior, estimating clock rate from a uniform prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca.taxa <- get_taxa_names(fasta_filename)
  mrca.taxa <- mrca.taxa[2:length(mrca.taxa)]
  mrca.prior <- create_mrca_prior(taxa_names=mrca.taxa,is_monophyletic = T)
  clock.rate <- beautier::create_clock_rate_param(value = "0.0035",estimate=TRUE)
  clock.uniform<-beautier::create_uniform_distr(value = 0.0035,lower = 0.00277, upper = 0.00542)

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA,clock.rate,clock.uniform),
    tree_prior = create_yule_tree_prior(),
    mrca_prior = mrca.prior,
    beauti_options = beautier::create_beauti_options_v2_6(
      nucleotides_uppercase = TRUE
    )
  )

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model= inference_model
  )


  # Make the inference model match the BEAUti file
  inference_model$tree_prior$birth_rate_distr$id <- "1"
  inference_model$site_model$kappa_prior_distr$m$id <- "1"
  inference_model$site_model$kappa_prior_distr$s$id <- "2"
  inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id <- "3"
  inference_model$clock_model$clock_rate_distr$id <- "0"

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  # If this passes, this is done!
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))

  beauti_text <- readr::read_lines(beauti_file)
  beautier_text <- readr::read_lines(beautier_file)
  if (1 + 1 == 2) {
    beautier::compare_lines(
      lines = beautier_text,
      expected = beauti_text,
      created_lines_filename = "~/created.xml",
      expected_lines_filename = "~/expected.xml"
    )
  }

  # example fix
  clock_rate_param_pattern <- "<branchRateModel id=.StrictClock.c:anthus_aco_sub. spec=.beast.evolution.branchratemodel.StrictClockModel. clock.rate=.@clockRate.c:anthus_aco_sub./>"
  expect_equal(1, length(stringr::str_subset(beauti_text, clock_rate_param_pattern)))
  expect_equal(1, length(stringr::str_subset(beautier_text, clock_rate_param_pattern)))
  beautier::remove_beautier_folder()

})
