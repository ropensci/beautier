context("create_beast2_input_state")

test_that("birth_death", {

  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = "test_output_0",
      site_models = list(create_jc69_site_model()),
      clock_models = beautier:::initialize_clock_models(
        list(create_strict_clock_model()), distr_id = 0),
      tree_priors = beautier:::initialize_tree_priors(
        list(create_yule_tree_prior()), distr_id = 1),
      initial_phylogenies = NA
    )
  )
})

test_that("abuse", {

  ids <- c("a", "b")

  # Two ids OK
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(n = 2),
      clock_models = create_strict_clock_models(n = 2),
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 2)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one site model
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(n = 1), # One too little
      clock_models = create_strict_clock_models(n = 2),
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 2)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one clock model
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(n = 2),
      clock_models = create_strict_clock_models(n = 1), # One too little
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 2)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one tree prior
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(n = 2),
      clock_models = create_strict_clock_models(n = 2),
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 1)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one phylogeny
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(n = 2),
      clock_models = create_strict_clock_models(n = 2),
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 2)),
      initial_phylogenies = rep(NA, 1)
    )
  )

})

test_that("use without initial phylogeny", {

  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = "test_output_0",
      site_models = list(create_jc69_site_model()),
      clock_models = list(create_strict_clock_model()),
      tree_priors = initialize_tree_priors(list(create_yule_tree_prior())),
      initial_phylogenies = NA
    )
  )

})

test_that("use one with initial phylogeny", {

  # 'c' is used to convert phylo to multiPhylo
  phylos <- c(
    fasta_to_phylo(
      get_input_fasta_filename(),
      crown_age = 42
    )
  )
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = "test_output_0",
      site_models = list(create_jc69_site_model()),
      clock_models = list(create_strict_clock_model()),
      tree_priors = initialize_tree_priors(list(create_yule_tree_prior())),
      initial_phylogenies = phylos
    )
  )

})

test_that("two phylogenies", {

  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = c("Anthus_nd2", "Anthus_aco"),
      site_models = create_jc69_site_models(n = 2),
      clock_models = create_strict_clock_models(n = 2),
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 2)),
      initial_phylogenies = c(NA, NA)
    )
  )


})

test_that("two alignments, two initial phylogenies", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  ids <- get_ids(c(fasta_filename_1, fasta_filename_2))
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 314)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 42)
  initial_phylogenies <- c(phylo1, phylo2)

  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(n = 2),
      clock_models = create_strict_clock_models(n = 2),
      tree_priors = initialize_tree_priors(create_yule_tree_priors(n = 2)),
      initial_phylogenies = initial_phylogenies
    )
  )

})
