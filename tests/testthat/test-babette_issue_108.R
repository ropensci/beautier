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

  # Tipdates end up in output file
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

  # Reproduce file
  readr::write_lines(beast2_xml_lines, "~/created.xml")
  readr::write_lines(expected_beast2_xml_lines, "~/expected.xml")
  beastier::are_beast2_input_lines_deep(beast2_xml_lines)

  # First step of fixing #108
  beautier::are_equivalent_xml_files(
    output_filename,
    expected_output_filename
  )

  remove_beautier_folder()
})

test_that("Reproduce file", {

  if (!"beastier" %in% installed.packages()[,1]) {
    return()
  }

  fasta_filename_beautier <- get_beautier_path("babette_issue_108.fasta")
  # Copy to have 1996-2000 in the data id
  fasta_filename <- file.path(
    rappdirs::user_cache_dir(appname = "beautier"),
    "1996-2000.fasta"
  )
  file.copy(from = fasta_filename_beautier, to = fasta_filename)


  tipdates_filename <- get_beautier_path("babette_issue_108_tipdates.txt")
  output_filename <- get_beautier_tempfilename()
  expected_output_filename <- get_beautier_path(
    "babette_issue_108_expected_20250203.xml"
  )
  # Shallow check works
  expect_true(
    beastier::are_beast2_input_lines(
      readr::read_lines(expected_output_filename)
    )
  )
  if (1 == 2) {
    # Deep check fails
    # Fix https://github.com/ropensci/beastier/issues/72
    beastier::are_beast2_input_lines_deep(
      readr::read_lines(expected_output_filename), verbose = TRUE

    )
  }

  create_beast2_input_file(
    input_filename = fasta_filename,
    output_filename = output_filename,
    tipdates_filename = tipdates_filename,
    clock_model = beautier::create_rln_clock_model(mean_clock_rate = "1.0"),
    site_model = beautier::create_hky_site_model(),
    mcmc = beautier::create_mcmc(),
    tree_prior = beautier::create_cbs_tree_prior(),
    beauti_options = beautier::create_beauti_options(beast2_version = "2.5")
  )

  # Reproduce file
  beast2_xml_lines <- readr::read_lines(output_filename)
  expected_beast2_xml_lines <- readr::read_lines(expected_output_filename)
  readr::write_lines(beast2_xml_lines, "~/created.xml")
  readr::write_lines(expected_beast2_xml_lines, "~/expected.xml")

  # First step of fixing #108
  beautier::are_equivalent_xml_lines(
    beast2_xml_lines,
    expected_beast2_xml_lines
  )


  if (1 == 2) {
    # Part of user-submitted code, not needed to
    # reproduce BEAST2 XML input file
    assignInNamespace(
      "get_beast2_main_class_name",
      function() {
          "beast.app.beastapp.BeastMain"
      },
      ns = "beastier"
    )
    run_beast2(
      input_filename = glue("{start}-{end}.xml"),
      output_state_filename = create_temp_state_filename(),
      use_beagle = TRUE,
      beast2_path = "/home/zhulab/.beast/2.5/BEAST/lib/beast.jar",
      verbose = T
    )
  }

  remove_beautier_folder()
})
