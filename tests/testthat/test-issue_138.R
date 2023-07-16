test_that("Re-create v2.6.7 BEAUti file, as created by Richel", {

  # Delivered by the user, not supported
  beauti_file <- beautier::get_beautier_path("issue_138_v2_6_7.xml")

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
      clock_rate_distr = create_normal_distr(
        id = NA,
        mean = mutation_rate,
        sigma = (mutation_rate / 10) * 2
      )
    ),
    tree_prior = create_cbs_tree_prior(),
    beauti_options = create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.base.evolution.alignment:beast.pkgmgmt:beast.base.core:beast.base.inference:beast.base.evolution.tree.coalescent:beast.pkgmgmt:beast.base.core:beast.base.inference.util:beast.evolution.nuc:beast.base.evolution.operator:beast.base.inference.operator:beast.base.evolution.sitemodel:beast.base.evolution.substitutionmodel:beast.base.evolution.likelihood",
      nucleotides_uppercase = TRUE
    )
  )

  create_beast2_input_file_from_model(
    input_filename = fasta_filename,
    output_filename = beautier_file,
    inference_model = inference_model
  )
  skip("Issue #138")
  beauti_text <- readr::read_lines(beauti_file)
  beautier_text <- readr::read_lines(beautier_file)
  compare_lines(
    lines = beautier_text,
    expected = beauti_text,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
  beautier::remove_beautier_folder()

})

test_that("Re-create v2.7 BEAUti file, as supplied by the user", {

  # Delivered by the user, not supported
  beauti_file <- beautier::get_beautier_path("issue_138_v2_7.xml")

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
  beauti_text <- readr::read_lines(beauti_file)
  beautier_text <- readr::read_lines(beautier_file)
  compare_lines(
    lines = beautier_text,
    expected = beauti_text,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )
  expect_equal(created, expected)


  expect_true(beautier::are_equivalent_xml_files(beauti_file, beautier_file))
  beautier::remove_beautier_folder()

})
