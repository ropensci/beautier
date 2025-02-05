test_that("lack of tipdates file must be detected", {
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

test_that("tipdates file must be used in the created file", {
  fasta_filename <- get_beautier_path("babette_issue_108.fasta")
  output_filename <- get_beautier_tempfilename()
  tipdates_filename <- get_beautier_path("babette_issue_108_tipdates.txt")
  expected_output_filename <- get_beautier_path("babette_issue_108_expected.xml")
  tipdates_table <- readr::read_tsv(
    tipdates_filename,
    col_names = c("A", "year"),
    show_col_types = FALSE
  )
  expect_silent(
    create_beast2_input_file(
      input_filename = fasta_filename,
      output_filename,
      tipdates_filename = tipdates_filename,
      clock_model = create_rln_clock_model(),
      site_model = create_gtr_site_model(),
      mcmc = create_mcmc(chain_length = 10000000),
      tree_prior = create_cbs_tree_prior(),
      beauti_options = create_beauti_options_v2_6(nucleotides_uppercase = TRUE)
    )
  )
  expect_true(file.exists(output_filename))
  beast2_xml_lines <- readr::read_lines(output_filename)
  expected_beast2_xml_lines <- readr::read_lines(expected_output_filename)



  expect_true(
    length(
      stringr::str_subset(
        beast2_xml_lines,
        pattern = "1996"
      )
    ) > 0
  )

  # readr::write_lines(beast2_xml_lines, "~/created.xml")
  # readr::write_lines(expected_beast2_xml_lines, "~/expected.xml")
  # beastier::are_beast2_input_lines_deep(beast2_xml_lines)

  remove_beautier_folder()
})
