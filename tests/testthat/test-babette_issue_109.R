test_that("tipdates file must be used in the created file", {

  if (!"beastier" %in% installed.packages()[,1]) {
    return()
  }

  #fasta_file <- "./combined_aligned_myx.fasta"
  fasta_filename <- get_beautier_path("babette_issue_108.fasta")

  #tipdates_filename <- "./tipdate.tsv"
  tipdates_filename <- get_beautier_path("babette_issue_108_tipdates.txt")

  #output_xml <- "out.aust.relaxed.constant.xlm"
  output_filename <- get_beautier_tempfilename()

  # --- Step: Define evolutionary models ---
  site_model <- create_hky_site_model()
  clock_model <- create_rln_clock_model() #relaxed log normal
  tree_prior <- create_ccp_tree_prior()  #coalescent constant tree prior

  mcmc <- create_mcmc()

  inference_model <- create_inference_model(
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mcmc = mcmc,
    tipdates_filename = tipdates_filename
  )

  testthat::expect_equal(
    1,
    length(stringr::str_subset(readr::read_lines(tipdates_filename), "1996.5"))
  )

  create_beast2_input_file(
    input_filename = fasta_filename,
    output_filename = output_filename,
    site_model = site_model,
    clock_model = clock_model,
    tree_prior = tree_prior,
    mcmc = mcmc
  )

  text <- readr::read_lines(output_filename)

  testthat::expect_equal(
    1,
    length(stringr::str_subset(text, "1996.5"))
  )

  remove_beautier_folder()
})
