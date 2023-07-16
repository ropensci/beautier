test_that("Try it to run", {

  # Delivered by the user
  beauti_file <- beautier::get_beautier_path("issue_138.xml")

  # Output file
  beautier_file <- get_beautier_tempfilename()

  # Delivered by the user
  fasta_filename <- get_beautier_path("Heleioporus_species_ND2_Pop1.fasta")
  mutation_rate <- 1.45E-08

  inference_model <- create_inference_model(
    site_model = create_hky_site_model(),
     # below sets a clock model with mean at empirical rate and a narrow standard deviation
    clock_model = create_strict_clock_model(
      id = NA,
      clock_rate_param = mutation_rate,
      clock_rate_distr = create_normal_distr(id = NA,
      mean = mutation_rate,
      sigma = (mutation_rate / 10) * 2)
    ),
    tree_prior = create_cbs_tree_prior(),
    mcmc = create_mcmc(
      chain_length = 20000000,
      tracelog = create_tracelog(log_every = 20000),
      screenlog = create_screenlog(log_every = 1000000),
      treelog = create_treelog(log_every = 20000)
    )
  )

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  skip("Issue #138")
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
  beautier::remove_beautier_folder()
})
