test_that("section of anthus_aco_sub_calibration.xml", {
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")
  inference_model <- init_inference_model(
    input_filename = fasta_filename,
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      mcmc = create_mcmc(chain_length = 10000),
      mrca_prior = create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        mrca_distr = create_normal_distr(
          id = 0,
          mean = create_mean_param(id = 1, value = "0.02"),
          sigma = create_sigma_param(id = 2, value = "0.001")
        ),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      ),
      beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
    )
  )
  expect_warning(
    mrca_prior_to_xml_lh_distr(
     inference_model = inference_model
    ),
    "'mrca_prior_to_xml_lh_distr' is deprecated"
  )
})


test_that("deprecation", {
  expect_error(
    mrca_prior_to_xml_lh_distr(
      mrca_prior = "something",
      inference_model = "irrelevant"
    ),
    "'mrca_prior' is deprecated, use 'inference_model' instead"
  )
  expect_error(
    mrca_prior_to_xml_lh_distr(
      has_non_strict_clock_model = "something",
      inference_model = "irrelevant"
    ),
    "'has_non_strict_clock_model' is deprecated, use 'inference_model' instead"
  )
})
