test_that("1: can re-create file 'issue_135_no_mrca_no_estimate_beauti.xml'", {
  beauti_file <- beautier::get_beautier_path(
    "issue_135_no_mrca_no_estimate_beauti.xml"
  )

  beautier_file <- get_beautier_tempfilename()

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")
  clock_rate <- beautier::create_clock_rate_param(
    value = 0.00277,
    estimate = FALSE
  )

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA, clock_rate),
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood", # nolint indeed a long line
      nucleotides_uppercase = TRUE
    )
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
  beautier::remove_beautier_folder()
})

test_that("2: can re-create file 'issue_135_no_mrca_estimate_beauti.xml'", {

  # Delivered by the user
  beauti_file <- beautier::get_beautier_path(
    "issue_135_no_mrca_estimate_beauti.xml"
  )

  beautier_file <- get_beautier_tempfilename()

  #134 without mrca prior, estimating clock rate from a uniform prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  clock_rate <- beautier::create_clock_rate_param(
    value = "0.0035", estimate = TRUE
  )
  clock_uniform <- beautier::create_uniform_distr(
    value = 0.0035, lower = 0.00277, upper = 0.00542
  )

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(
      id = NA,
      clock_rate_param = clock_rate,
      clock_rate_distr = clock_uniform
    ),
    tree_prior = create_yule_tree_prior(),
    beauti_options = beautier::create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood", # nolint indeed a long line
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
  beauti_file <- beautier::get_beautier_path(
    "issue_135_mrca_no_estimate_beauti.xml"
  )
  beautier_file <- get_beautier_tempfilename()

  #134 without mrca prior, estimating clock rate from a uniform prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  #With mrca prior, single value at clock rate
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca_taxa <- get_taxa_names(fasta_filename)
  mrca_taxa <- mrca_taxa[2:length(mrca_taxa)]
  mrca_prior <- create_mrca_prior(
    taxa_names = mrca_taxa,
    is_monophyletic = TRUE
  )
  clock_rate <- beautier::create_clock_rate_param(
    value = 0.00277,
    estimate = FALSE
  )

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(id = NA, clock_rate),
    tree_prior = create_yule_tree_prior(),
    mrca_prior = mrca_prior,
    beauti_options = beautier::create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood", # nolint indeed a long line
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
  beautier::remove_beautier_folder()
})


test_that("4: can re-create file 'issue_135_mrca_estimate_beauti.xml'", {

  # This is a unique file, delivered by the user
  beauti_file <- beautier::get_beautier_path(
    "issue_135_mrca_estimate_beauti.xml"
  )
  beautier_file <- get_beautier_tempfilename()

  #With mrca prior, estimating clock rate from a uniform prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  mrca_taxa <- get_taxa_names(fasta_filename)
  mrca_taxa <- mrca_taxa[2:length(mrca_taxa)]
  mrca_prior <- create_mrca_prior(
    taxa_names = mrca_taxa,
    is_monophyletic = TRUE
  )
  clock_rate <- beautier::create_clock_rate_param(
    value = "0.0035",
    estimate = TRUE
  )
  clock_uniform <- beautier::create_uniform_distr(
    value = 0.0035, lower = 0.00277, upper = 0.00542
  )

  inference_model <- create_inference_model(
    site_model = beautier::create_hky_site_model(),
    clock_model = beautier::create_strict_clock_model(
      id = NA,
      clock_rate,
      clock_uniform
    ),
    tree_prior = create_yule_tree_prior(),
    mrca_prior = mrca_prior,
    beauti_options = beautier::create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood", # nolint indeed a long line
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
  # If this passes, this is done!
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
  beautier::remove_beautier_folder()
})
