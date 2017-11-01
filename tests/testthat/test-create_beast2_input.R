context("create_beast2_input")

################################################################################
# General
################################################################################

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

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      site_models = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      tree_priors = "nonsense"
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      clock_models = "nonsense"
    )
  )

})

test_that("Check that 2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename()
  )

  expected_lines <- readLines(system.file("extdata",
    "2_4.xml", package = "beautier"))

  if (1 == 2) { # nolint keep this to help fixing future tests
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

################################################################################
# Site models
################################################################################

################################################################################
# Site model: GTR
################################################################################


test_that("Check that gtr_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_site_model(name = "GTR")
  )

  expected_lines <- readLines(system.file("extdata",
    "gtr_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_1_2_4.xml"), {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 1
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "gtr_gcc_1_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_2_2_4.xml"), {

  skip("WIP: do gtr_gcc_2.xml first")
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "gtr_gcc_2_2_4.xml", package = "beautier"))

  if (1 == 2) { # nolint keep this to help fixing future tests
    write.csv(created_lines, "~/created.csv")
    write.csv(expected_lines, "~/expected.csv")
    for (i in 1:min(length(expected_lines), length(created_lines))) {
      testthat::expect_equal(
        expected_lines[i], created_lines[i]
      )
      print(paste0(i, " / ", length(expected_lines)))
    }
  }
  are_equivalent_xml_lines(expected_lines, created_lines, verbose = TRUE)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_2_shape_1_5_2_4.xml"), {

  skip("WIP: do gtr_gcc_2.xml first")
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "gtr_gcc_2_shape_1_5_2_4.xml", package = "beautier"))

  if (1 == 2) { # nolint keep this to help fixing future tests
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

test_that(paste0("Reproduce gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"), {

  skip("WIP: do gtr_gcc_2.xml first")
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5,
        prop_invariant = 0.5
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", package = "beautier"))

  if (1 == 2) { # nolint keep this to help fixing future tests
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

################################################################################
# Site model: HKY
################################################################################

test_that("Check that hky_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_site_model(name = "HKY")
  )

  expected_lines <- readLines(system.file("extdata",
    "hky_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})



test_that("Check that hky_kappa_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(kappa = 3.4)
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_kappa_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("Check that hky_prop_invariant_0_5_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      gamma_site_model = create_gamma_site_model(
        prop_invariant = 0.5
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_prop_invariant_0_5_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("Check that hky_gcc_1_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 1
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_gcc_1_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that hky_gcc_2_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_gcc_2_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that hky_gcc_4_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 4
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_gcc_4_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Site model: JC69
################################################################################

test_that("Check that jc69_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_site_model(name = "JC69")
  )

  expected_lines <- readLines(system.file("extdata",
    "jc69_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce jc69_gcc_2_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_jc69_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "jc69_gcc_2_2_4.xml", package = "beautier"))

  if (1 == 2) { # nolint keep this to help fixing future tests
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

test_that("Check that jc69_gcc_2_shape_1_5_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_jc69_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "jc69_gcc_2_shape_1_5_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Check that jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml",
  " is reproduced"), {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_jc69_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5,
        prop_invariant = 0.5
      )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml",
    package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Site model: TN93
################################################################################

test_that("Check that tn93_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_site_model(name = "TN93")
  )

  expected_lines <- readLines(system.file("extdata",
    "tn93_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Clock models
################################################################################

################################################################################
# Clock model: RLN
################################################################################

test_that("Check that relaxed_clock_log_normal_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_clock_model(name = "relaxed_log_normal")
  )
  expected_lines <- readLines(system.file("extdata",
    "relaxed_clock_log_normal_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Clock model: strict
################################################################################

test_that("Check that strict_clock_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_clock_model(name = "strict")
  )

  expected_lines <- readLines(system.file("extdata",
    "strict_clock_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that strict_clock_rate_0_5_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_strict_clock_model(
      rate = 0.5
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "strict_clock_rate_0_5_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Tree priors
################################################################################

################################################################################
# Tree prior: BD
################################################################################

test_that("Check that birth_death_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_tree_prior(name = "birth_death")
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_death_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})


################################################################################
# Tree prior: CBS
################################################################################

test_that("Check that coalescent_bayesian_skyline_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_tree_prior(
      name = "coalescent_bayesian_skyline")
  )

  expected_lines <- readLines(system.file("extdata",
    "coalescent_bayesian_skyline_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that coalescent_bayesian_skyline_2_4.xml is invalid", {

  # coalescent_bayesian_skyline_2_4.xml is invalid,
  # because the groupSize's dimension is 5 by default,
  # where the supplied number of taxa is 5. 5 taxa, this 4 nodes, so
  # groupSize cannot be more than 4
  filename <- system.file("extdata",
    "coalescent_bayesian_skyline_2_4.xml", package = "beautier")
  testthat::expect_false(is_beast2_input_file(filename))
})

################################################################################
# Tree prior: CCP
################################################################################

test_that("Check that coalescent_constant_population_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_tree_prior(
      name = "coalescent_constant_population")
  )

  expected_lines <- readLines(system.file("extdata",
    "coalescent_constant_population_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Tree prior: Yule
################################################################################

test_that("Check that yule_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_tree_prior(name = "yule")
  )

  expected_lines <- readLines(system.file("extdata",
    "yule_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})


################################################################################
# Initial phylogenies
################################################################################

test_that("Check that anthus_2_4.xml is reproduced", {

  skip("WIP")
  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    misc_options = create_misc_options(
      capitalize_first_char_id = TRUE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "anthus_2_4.xml", package = "beautier"))

  if (1 == 2) { # nolint keep this to help fixing future tests
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
