context("create_beast2_input_operators")

test_that("abuse", {

  id <- "test_output_0"

  testthat::expect_silent(
    beautier:::create_beast2_input_operators(
      site_models = create_jc69_site_models(ids = id),
      clock_models = create_strict_clock_models(ids = id),
      tree_priors = create_yule_tree_priors(ids = id),
      fixed_crown_ages = FALSE
    )
  )

})

test_that("Operators that change crown age are absent at fixed crown age", {
  input_fasta_filename <- beautier::get_fasta_filename()
  id <- get_id(input_fasta_filename)
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  created_fixed <- beautier:::create_beast2_input_operators(
    site_models = create_jc69_site_models(ids = id),
    clock_models = create_strict_clock_models(ids = id),
    tree_priors = create_yule_tree_priors(ids = id),
    fixed_crown_ages = TRUE
  )

  created_nonfixed <- beautier:::create_beast2_input_operators(
    site_models = create_jc69_site_models(ids = id),
    clock_models = create_strict_clock_models(ids = id),
    tree_priors = create_yule_tree_priors(ids = id),
    fixed_crown_ages = FALSE
  )

  # treeScaler operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "TreeScaler",
      x = created_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "TreeScaler",
      x = created_fixed))
  )

  # wide operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "TreeRootScaler",
      x = created_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "TreeRootScaler",
      x = created_fixed))
  )

  # subtree slide operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "SubtreeSlide",
      x = created_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "SubtreeSlide",
      x = created_fixed))
  )
})

test_that("Multiple fixed_crown_ages, interface", {
  input_filenames <- get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
  ids <- get_ids(input_filenames)

  testthat::expect_silent(
    beautier:::create_beast2_input_operators(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      fixed_crown_ages = c(TRUE, TRUE)
    )
  )

})
