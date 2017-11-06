#' Create a BEAST2 posterior by running BEAST2 from a random alignment
#' @param n_taxa number of taxa in the simulated phylogeny
#' @param sequence_length a DNA sequence length, in base pairs
#' @param crown_age the crown age of the phylogeny
#' @inheritParams create_beast2_input
#' @author Richel J.C. Bilderbeek
#' @export
create_posterior <- function(
    n_taxa,
    sequence_length,
    tree_priors = create_tree_prior(name = "yule"),
    mcmc_chainlength = get_default_mcmc_chain_length(),
    fixed_crown_age = FALSE,
    crown_age = NA
) {
  if (n_taxa < 2) {
    stop("Must create at least two taxa")
  }
  if (sequence_length < 1) {
    stop("Must create a sequence of at least one nucleotide")
  }
  if (!is_tree_prior(tree_priors)) {
    stop("Must use a valid tree prior")
  }
  if (mcmc_chainlength < 10000) {
    stop("Must use an MCMC chain length of at least 10000")
  }
  if (!is.logical(fixed_crown_age)) {
    stop("fixed_crown_age must be either TRUE of FALSE")
  }
  if (is.numeric(crown_age) && crown_age <= 0.0) {
    stop("crown age must be either NA or non-zero postive")
  }
  if (fixed_crown_age == FALSE && is.numeric(crown_age)) {
    stop("Cannot specify a crown age if crown age is not fixed")
  }
  base_filename <- "tmp_create_posterior"
  # BEAST2 input XML file, created by beautier::create_beast2_input_file
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
  beautier::create_random_fasta(
    n_taxa = n_taxa,
    sequence_length = sequence_length,
    filename = input_fasta_filename
  )
  testthat::expect_true(file.exists(input_fasta_filename))

  initial_phylogenies <- NA
  if (fixed_crown_age == TRUE && !is.na(crown_age)) {
    # Use 'c' to create a multiPhylo
    initial_phylogenies <- c(
        fasta_to_phylo(
        fasta_filename = input_fasta_filename, crown_age = crown_age
      )
    )
  }
  testit::assert(length(initial_phylogenies) == length(input_fasta_filename))

  # Create BEAST2 input file
  testthat::expect_false(file.exists(beast_filename))
  beautier::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filename,
    mcmc_chainlength = mcmc_chainlength,
    tree_priors = create_tree_prior(name = "birth_death"),
    output_xml_filename = beast_filename,
    fixed_crown_age = fixed_crown_age,
    initial_phylogenies = initial_phylogenies
  )
  testthat::expect_true(file.exists(beast_filename))
  testthat::expect_true(
    beautier::is_beast2_input_file(beast_filename)
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
