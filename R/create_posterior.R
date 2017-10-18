#' Create a BEAST2 posterior by running BEAST2 from a random alignment
#' @author Richel Bilderbeek
#' @export
create_posterior <- function(
    n_taxa = 5,
    sequence_length = 10,
    mcmc_chainlength = 10000
) {
  #setwd(path.expand("~"))
  #set.seed(42)

  base_filename <- "tmp_create_posterior"
  # BEAST2 input XML file, created by beastscriptr::create_beast2_input_file
  beast_filename <- paste0(base_filename, ".xml")
  # BEAST2 output file, containing the posterior parameter estimates
  beast_log_filename <- paste0(base_filename, ".log")
  # BEAST2 output file, containing the posterior phylogenies
  beast_trees_filename <- paste0(base_filename, ".trees")
  # BEAST2 output file, containing the final MCMC state
  beast_state_filename <- paste0(base_filename, ".xml.state")
  # FASTA file needed only temporarily to store simulated DNA alignments
  input_fasta_filename <- paste0(base_filename, ".fasta")

  # Create FASTA file
  testthat::expect_false(file.exists(input_fasta_filename))
  beastscriptr::create_random_fasta(
    n_taxa = 5,
    sequence_length = 10,
    filename = input_fasta_filename
  )
  testthat::expect_true(file.exists(input_fasta_filename))

  # Create BEAST2 input file
  testthat::expect_false(file.exists(beast_filename))
  beastscriptr::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = 10000,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = beast_filename,
    fixed_crown_age = TRUE
  )
  testthat::expect_true(file.exists(beast_filename))
  testthat::expect_true(
    beastscriptr::is_beast2_input_file(beast_filename)
  )

  # Run BEAST2 to measure posterior
  testthat::expect_false(file.exists(beast_state_filename))
  testthat::expect_false(file.exists(beast_log_filename))
  testthat::expect_false(file.exists(beast_trees_filename))
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar",
    " -statefile ", beast_state_filename,
    " -overwrite", beast_filename
  )
  verbose <- FALSE
  if (!verbose) {
    cmd <- paste(cmd, "1>/dev/null 2>/dev/null")
  }
  system(cmd)
  # If these are absent, BEAST2 could not parse the input file
  testthat::expect_true(file.exists(beast_state_filename))
  testthat::expect_true(file.exists(beast_log_filename))
  testthat::expect_true(file.exists(beast_trees_filename))

  # All TreeHeights (crown ages) should be the same
  posterior <- RBeast::parse_beast_posterior(
    trees_filename = beast_trees_filename,
    log_filename = beast_log_filename)

  file.remove(beast_filename)
  file.remove(beast_state_filename)
  file.remove(beast_log_filename)
  file.remove(beast_trees_filename)
  file.remove(input_fasta_filename)

  posterior
}
