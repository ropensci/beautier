context("create_beast2_input_operators")

test_that("abuse", {

  testthat::expect_silent(
    beautier:::create_beast2_input_operators(
      ids = "test_output_0",
      fixed_crown_age = FALSE
    )
  )

  testthat::expect_error(
    beautier:::create_beast2_input_operators(
      ids = ape::rcoal(5),
      fixed_crown_age = FALSE
    )
  )

  testthat::expect_error(
    beautier:::create_beast2_input_operators(
      ids = "test_output_0",
      fixed_crown_age = "Nonsense"
    )
  )

testthat::expect_error(
    beautier:::create_beast2_input_operators(
      ids = "test_output_0",
      tree_priors = "Nonsense"
    )
  )

  # Two IDs, one site model
  testthat::expect_error(
    beautier:::create_beast2_input_operators(
      ids = c("a", "b"),
      site_models = create_jc69_site_model()
    )
  )

  # Two IDs, one clock model
  testthat::expect_error(
    beautier:::create_beast2_input_operators(
      ids = c("a", "b"),
      clock_models = create_strict_clock_model()
    )
  )

  # Two IDs, one tree prior
  testthat::expect_error(
    beautier:::create_beast2_input_operators(
      ids = c("a", "b"),
      clock_models = create_yule_tree_prior()
    )
  )


})

test_that("Operators that change crown age are absent at fixed crown age", {
  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  created_lines_fixed <- beautier:::create_beast2_input_operators(
    ids = id,
    tree_priors = create_yule_tree_priors(ids = id),
    fixed_crown_age = TRUE
  )

  created_lines_nonfixed <- beautier:::create_beast2_input_operators(
    ids = id,
    tree_priors = create_yule_tree_priors(ids = id),
    fixed_crown_age = FALSE
  )

  # treeScaler operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "TreeScaler",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "TreeScaler",
      x = created_lines_fixed))
  )

  # wide operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "TreeRootScaler",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "TreeRootScaler",
      x = created_lines_fixed))
  )

  # subtree slide operator absent in fixed crown age tree
  testthat::expect_equal(1,
    length(grep(pattern = "SubtreeSlide",
      x = created_lines_nonfixed))
  )
  testthat::expect_equal(0,
    length(grep(pattern = "SubtreeSlide",
      x = created_lines_fixed))
  )
})
