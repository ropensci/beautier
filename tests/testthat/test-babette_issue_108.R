test_that("use", {
  expect_true(file.exists(get_beautier_path("babette_issue_108_tipdates.txt")))
  expect_true(file.exists(get_beautier_path("babette_issue_108.fasta")))
  fasta_filename <- get_beautier_path("babette_issue_108.fasta")
  output_filename <- get_beautier_tempfilename()
  tipdates_filename <- "TIPDATES_FILENAME"
  expect_error(
    create_beast2_input_file(
      input_filename = fasta_filename,
      output_filename,
      tipdates_filename = tipdates_filename,
      clock_model = create_rln_clock_model(),
      site_model = create_gtr_site_model(),
      mcmc = create_mcmc(chain_length = 1000),
      tree_prior = create_cbs_tree_prior(),
      beauti_options = create_beauti_options_v2_6()
    ),
    "Tipdating filename not found at path 'TIPDATES_FILENAME'"
  )
  expect_false(file.exists(output_filename))
  expect_false(file.exists(tipdates_filename))
  remove_beautier_folder()
})
