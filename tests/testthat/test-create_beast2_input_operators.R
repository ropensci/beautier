context("create_beast2_input_operators")

test_that("usage", {
  testthat::expect_silent(
    create_beast2_input_operators(
      fasta_filenames = beastscriptr::get_input_fasta_filename(),
      tree_priors = create_tree_prior("birth_death"),
      fixed_crown_age = FALSE
    )
  )
})

test_that("Operators that change crown age are absent at fixed crown age", {
  input_fasta_filename <- beastscriptr::get_input_fasta_filename()
  testthat::expect_equal(file.exists(input_fasta_filename), TRUE)

  created_lines_fixed <- beastscriptr::create_beast2_input_operators(
    fasta_filenames = input_fasta_filename,
    tree_priors = create_tree_prior(name = "birth_death"),
    fixed_crown_age = TRUE
  )

  created_lines_nonfixed <- beastscriptr::create_beast2_input_operators(
    fasta_filenames = input_fasta_filename,
    tree_priors = create_tree_prior(name = "birth_death"),
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
