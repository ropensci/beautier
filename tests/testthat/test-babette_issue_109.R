test_that("tipdates file must be used in the created file", {

  if (!"beastier" %in% installed.packages()[,1]) {
    return()
  }
  if (1 == 2) {
    setwd("~/Downloads/file/")
  }

  #fasta_filename <- get_beautier_path("babette_issue_108.fasta")
  #fasta_filename <- get_beautier_path("THAILAND_TEST.clust_1.dated.fa")
  #fasta_filename <- "combined_aligned_myx.fasta"
  fasta_filename <- get_beautier_path("babette_issue_109.fasta")

  #tipdates_filename <- get_beautier_path("babette_issue_108_tipdates.txt")
  #tipdates_filename <- get_beautier_path("THAILAND_TEST.clust_1.dated.txt")
  #tipdates_filename <- "tipdate.tsv"
  tipdates_filename <- get_beautier_path("babette_issue_109.tsv")


  output_filename <- get_beautier_tempfilename()

  testthat::expect_equal(
    1,
    length(stringr::str_subset(readr::read_lines(tipdates_filename), "2014"))
  )

  # Inference model that works
  inference_model <- create_inference_model(
    site_model = create_gtr_site_model(),
    clock_model = create_rln_clock_model(),
    tree_prior = create_yule_tree_prior(),
    mcmc = create_mcmc(),
    tipdates_filename = tipdates_filename,
    beauti_options = create_beauti_options_v2_6()
  )

  create_beast2_input_file_from_model(
    inference_model = inference_model,
    input_filename = fasta_filename,
    output_filename = output_filename
  )

  text <- readr::read_lines(output_filename)

  testthat::expect_equal(
    1,
    length(stringr::str_subset(text, "2014"))
  )

  # Inference model that fails
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

  text <- readr::read_lines(output_filename)

  testthat::expect_equal(
    1,
    length(stringr::str_subset(text, "2014"))
  )

  remove_beautier_folder()
})
