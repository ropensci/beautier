#' Create a BEAST2 posterior by running BEAST2 from a random alignment
#' @inheritParams default_params_doc
#' @param n_taxa number of taxa in the simulated phylogeny
#' @author Richel J.C. Bilderbeek
create_posterior <- function(
  n_taxa,
  sequence_length,
  tree_priors = create_yule_tree_prior(),
  mcmc = create_mcmc(),
  crown_ages = NA,
  verbose = FALSE
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
  if (!is_mcmc(mcmc)) {
    stop("Must use a valid mcmc object")
  }
  for (crown_age in crown_ages) {
    if (!is.na(crown_age) && !is.numeric(crown_age)) {
      stop("crown age must be either NA or a non-zero positive number")
    }
    if (is.numeric(crown_age) && crown_age <= 0.0) {
      stop("crown age must be either NA or a non-zero positive number")
    }
  }
  base_filename <- "tmp_create_posterior"
  #base_filename <- tempfile(pattern = "tmp_create_posterior")
  # BEAST2 input XML file, created by beautier::create_beast2_input_file
  beast_filename <- paste0(base_filename, ".xml")
  # BEAST2 output file, containing the posterior parameter estimates
  beast_log_filename <- paste0(base_filename, ".log")
  # BEAST2 output file, containing the posterior phylogenies
  beast_trees_filename <- paste0(base_filename, ".trees")
  # BEAST2 output file, containing the final MCMC state
  beast_state_filename <- paste0(base_filename, ".xml.state")
  # FASTA file needed only temporarily to store simulated DNA alignments
  input_fasta_filenames <- paste0(paste0(base_filename, "_", seq_along(crown_ages)), ".fasta")
  input_fasta_filenames[1] <- paste0(base_filename, ".fasta")
  print(paste("input_fasta_filenames: ", input_fasta_filenames))

  # Create FASTA file
  create_random_fastas(
    n_taxa = n_taxa,
    sequence_length = sequence_length,
    fasta_filenames = input_fasta_filenames
  )
  testthat::expect_true(files_exist(input_fasta_filenames))

  initial_phylogenies <- list()
  for (i in seq_along(crown_ages)) {
    crown_age <- crown_ages[i]
    if (is.na(crown_age)) {
      initial_phylogenies[[i]] <- NA
      next
    }
    testit::assert(crown_age > 0.0)
    initial_phylogenies[[i]] <- fasta_to_phylo(
      fasta_filename = input_fasta_filenames[i],
      crown_age = crown_age
    )
  }
  testit::assert(length(input_fasta_filenames) == length(initial_phylogenies))
  testit::assert(length(input_fasta_filenames) == length(crown_ages))
  fixed_crown_ages <- !is.na(crown_ages)
  print(paste("fixed_crown_ages:", fixed_crown_ages))
  remove_files(c(beast_filename))

  # Create BEAST2 input file
  testthat::expect_false(file.exists(beast_filename))
  beautier::create_beast2_input_file(
    input_fasta_filenames = input_fasta_filenames,
    mcmc = mcmc,
    output_xml_filename = beast_filename,
    fixed_crown_ages = fixed_crown_ages,
    initial_phylogenies = initial_phylogenies
  )
  print(paste("beast_filename:", beast_filename))
  testthat::expect_true(file.exists(beast_filename))
  testthat::expect_true(
    is_beast2_input_file(beast_filename)
  )

  # Run BEAST2 to measure posterior
  remove_files(c(beast_state_filename, beast_log_filename, beast_trees_filename))
  testthat::expect_false(files_exist(c(beast_state_filename, beast_log_filename, beast_trees_filename)))
  cmd <- paste(
    "java -jar ~/Programs/beast/lib/beast.jar",
    " -statefile ", beast_state_filename,
    " -overwrite", beast_filename
  )
  if (!verbose) {
    cmd <- paste(cmd, "1>/dev/null 2>/dev/null")
  }
  system(cmd)
  # If these are absent, BEAST2 could not parse the input file
  print(paste("beast_state_filename:", beast_state_filename))
  testthat::expect_true(file.exists(beast_state_filename))
  print(paste("beast_log_filename:", beast_log_filename))
  testthat::expect_true(file.exists(beast_log_filename))
  testthat::expect_true(file.exists(beast_trees_filename))

  # All TreeHeights (crown ages) should be the same
  posterior <- beastier::parse_beast_posterior(
    trees_filename = beast_trees_filename,
    log_filename = beast_log_filename)

  file.remove(beast_filename)
  file.remove(beast_state_filename)
  file.remove(beast_log_filename)
  file.remove(beast_trees_filename)
  file.remove(input_fasta_filename)

  posterior
}
