context("create_beast2_input")

test_that("checks input", {

  testthat::expect_silent(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename()
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = "nonexisting" # Error
    )
  )
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc_chainlength = 0 # Error
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      tree_priors = create_tree_prior(name = "nonsense")
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      fixed_crown_age = "nonsense" # Error
    )
  )

})

test_that("Check that 2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename()
  )

  expected_lines <- readLines(system.file("extdata", "2_4.xml", package = "beastscriptr"))
  testthat::expect_identical(created_lines, expected_lines)
})


test_that("Check that birth_death_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    tree_priors = beastscriptr::create_tree_prior(name = "birth_death")
  )

  expected_lines <- readLines(system.file("extdata", "birth_death_2_4.xml", package = "beastscriptr"))

  testthat::expect_identical(created_lines, expected_lines)
})


test_that("Check that coalescent_constant_population_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    tree_priors = beastscriptr::create_tree_prior(name = "coalescent_constant_population")
  )

  expected_lines <- readLines(system.file("extdata", "coalescent_constant_population_2_4.xml", package = "beastscriptr"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that yule_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    tree_priors = beastscriptr::create_tree_prior(name = "yule")
  )

  expected_lines <- readLines(system.file("extdata", "yule_2_4.xml", package = "beastscriptr"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that coalescent_bayesian_skyline_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    tree_priors = beastscriptr::create_tree_prior(name = "coalescent_bayesian_skyline")
  )

  expected_lines <- readLines(system.file("extdata", "coalescent_bayesian_skyline_2_4.xml", package = "beastscriptr"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that jc69_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    site_models = create_site_model(name = "JC69")
  )

  expected_lines <- readLines(system.file("extdata", "jc69_2_4.xml", package = "beastscriptr"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that hky_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    site_models = create_site_model(name = "HKY")
  )

  expected_lines <- readLines(system.file("extdata", "hky_2_4.xml", package = "beastscriptr"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that tn93_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    site_models = create_site_model(name = "TN93")
  )

  expected_lines <- readLines(system.file("extdata", "tn93_2_4.xml", package = "beastscriptr"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that gtr_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    site_models = create_site_model(name = "GTR")
  )

  expected_lines <- readLines(system.file("extdata", "gtr_2_4.xml", package = "beastscriptr"))
  testthat::expect_identical(created_lines, expected_lines)
})


test_that("Check that strict_clock_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    clock_models = create_clock_model(name = "strict")
  )

  expected_lines <- readLines(system.file("extdata", "strict_clock_2_4.xml", package = "beastscriptr"))
  testthat::expect_identical(created_lines, expected_lines)
})





test_that("Check that relaxed_clock_log_normal_2_4.xml is reproduced", {

  created_lines <- beastscriptr::create_beast2_input(
    input_fasta_filenames = beastscriptr::get_input_fasta_filename(),
    clock_models = create_clock_model(name = "relaxed_log_normal")
  )

  expected_lines <- readLines(system.file("extdata", "relaxed_clock_log_normal_2_4.xml", package = "beastscriptr"))

  if (1 == 2) {
    write.csv(created_lines, "~/created.csv")
    write.csv(expected_lines, "~/expected.csv")
    for (i in 1:min(length(expected_lines), length(created_lines))) {
      testthat::expect_equal(
        expected_lines[i], created_lines[i]
      )
      print(paste0(i, " / ", length(expected_lines)))
    }
  }

  testthat::expect_identical(created_lines, expected_lines)
})
