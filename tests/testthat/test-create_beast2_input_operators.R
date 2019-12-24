context("create_beast2_input_operators")

test_that("abuse", {

  id <- "test_output_0"

  testthat::expect_silent(
    create_beast2_input_operators(
      site_models = list(create_jc69_site_model(id = id)),
      clock_models = list(create_strict_clock_model(id = id)),
      tree_priors = list(create_yule_tree_prior(id = id)),
      fixed_crown_ages = FALSE
    )
  )
})

test_that("Operators that change crown age are absent at fixed crown age", {
  input_fasta_filename <- beautier::get_fasta_filename()
  id <- get_alignment_id(input_fasta_filename)
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  created_fixed <- create_beast2_input_operators(
    site_models = list(create_jc69_site_model(id = id)),
    clock_models = list(create_strict_clock_model(id = id)),
    tree_priors = list(create_yule_tree_prior(id = id)),
    fixed_crown_ages = TRUE
  )

  created_nonfixed <- create_beast2_input_operators(
    site_models = list(create_jc69_site_model(id = id)),
    clock_models = list(create_strict_clock_model(id = id)),
    tree_priors = list(create_yule_tree_prior(id = id)),
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
