context("create_beast2_input_state")

test_that("birth_death", {

  id <- "test_output_0"
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = id,
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = beautier:::init_clock_models(
        list(create_strict_clock_model()), ids = id, distr_id = 0),
      tree_priors = beautier:::init_tree_priors(
        list(create_yule_tree_prior()), distr_id = 1),
      initial_phylogenies = NA
    )
  )

  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = id,
      site_models = list(create_gtr_site_model(id = id)),
      clock_models = beautier:::init_clock_models(
        list(create_strict_clock_model()), ids = id, distr_id = 0),
      tree_priors = beautier:::init_tree_priors(
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
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one site model
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = "only_one"),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one clock model
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(
        ids = "only_one"), # One too little
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one tree prior
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = "only_one")),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one phylogeny
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 1)
    )
  )

})

test_that("use without initial phylogeny", {

  id <- "test_output_0"
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = id,
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = list(create_strict_clock_model()),
      tree_priors = beautier:::init_tree_priors(
        list(create_yule_tree_prior())
      ),
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
  id <- "test_output_0"
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = id,
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = list(create_strict_clock_model()),
      tree_priors = beautier:::init_tree_priors(list(
        create_yule_tree_prior())
      ),
      initial_phylogenies = phylos
    )
  )

})

test_that("two phylogenies", {

  ids <- c("Anthus_nd2", "Anthus_aco")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = c(NA, NA)
    )
  )

})

test_that("three phylogenies", {

  ids <- c("aco", "nd2", "nd3")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = rep(NA, length(ids))
    )
  )

})

test_that("four phylogenies", {

  ids <- c("aco", "nd2", "nd3", "nd4")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = rep(NA, length(ids))
    )
  )

})

test_that("four phylogenies, shared clock", {

  skip("WIP")

  ids <- c("aco", "nd2", "nd3", "nd4")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      ids = ids,
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids[1]),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = rep(NA, length(ids))
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
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = initial_phylogenies
    )
  )

})
