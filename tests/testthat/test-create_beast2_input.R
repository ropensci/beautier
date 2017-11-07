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
      mcmc = create_mcmc(chain_length = 0) # Error
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

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc = "nonsense"
    )
  )

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")

  # Two filesnames, one site model
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      site_models = create_jc69_site_model()
    )
  )

  # Two filesnames, one clock model
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      clock_models = create_strict_model()
    )
  )

  # Two filesnames, one tree prior
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      tree_priors = create_yule_tree_prior()
    )
  )

  # Two filesnames, one phylogeny
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})

test_that("Reproduce 2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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


test_that("Reproduce gtr_2_4.xml", {


  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_site_model(name = "GTR"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "gtr_2_4.xml", package = "beautier"))

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

test_that(paste0("Reproduce gtr_gcc_1_2_4.xml"), {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 1
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "gtr_gcc_1_2_4.xml", package = "beautier"))

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

test_that(paste0("Reproduce gtr_gcc_2_2_4.xml"), {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_2_shape_1_5_2_4.xml"), {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5,
        prop_invariant = 0.5
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    site_models = create_site_model(name = "HKY"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "hky_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})



test_that("Check that hky_kappa_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(kappa = 3.4),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_gcc_1_2_4.xml", package = "beautier"))

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

test_that("Check that hky_gcc_2_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "hky_gcc_2_2_4.xml", package = "beautier"))

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

test_that("Check that hky_gcc_4_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 4
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    site_models = create_site_model(name = "JC69"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))

  )
  expected_lines <- readLines(system.file("extdata",
    "jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml",
    package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Site model: TN93
################################################################################

test_that("Reproduce tn93_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_tn93_site_model(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "tn93_2_4.xml", package = "beautier"))

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

test_that("Check that tn93_gcc_1_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_tn93_site_model(
      gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "tn93_gcc_1_2_4.xml", package = "beautier"))

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

test_that("Reproduce tn93_gcc_2_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_tn93_site_model(
      gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "tn93_gcc_2_2_4.xml", package = "beautier"))

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
# Clock models
################################################################################

################################################################################
# Clock model: RLN
################################################################################

test_that("Reproduce relaxed_clock_log_normal_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_rln_clock_model(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "relaxed_clock_log_normal_2_4.xml", package = "beautier"))


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
# Clock model: strict
################################################################################

test_that("Check that strict_clock_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_clock_model(name = "strict"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
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

test_that("Reproduce birth_death_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_tree_prior(name = "birth_death")
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_death_2_4.xml", package = "beautier"))

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
    tree_priors = beautier::create_ccp_tree_prior()
  )

  expected_lines <- readLines(system.file("extdata",
    "coalescent_constant_population_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Tree prior: CEP
################################################################################

test_that("Reproduce coalescent_exponential_population_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_cep_tree_prior()
  )

  expected_lines <- readLines(system.file("extdata",
    "coalescent_exponential_population_2_4.xml", package = "beautier"))

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
# Tree prior: Yule
################################################################################

test_that("Reproduce yule_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "yule_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Priors
################################################################################
test_that("Reproduce birth_rate_uniform_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_prior = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_uniform_2_4.xml", package = "beautier"))

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

test_that("Reproduce birth_rate_normal_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_normal_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_normal_2_4.xml", package = "beautier"))


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



test_that("Reproduce birth_rate_one_div_x_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_one_div_x_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_one_div_x_2_4.xml", package = "beautier"))


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

test_that("Reproduce birth_rate_log_normal_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_log_normal_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_log_normal_2_4.xml", package = "beautier"))


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

test_that("Reproduce birth_rate_exponential_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_exponential_distr()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_exponential_2_4.xml", package = "beautier"))


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


test_that("Reproduce birth_rate_gamma_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_yule_tree_prior(
      birth_rate_distribution = beautier::create_gamma_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_gamma_2_4.xml", package = "beautier"))


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

test_that("Reproduce birth_rate_beta_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_beta_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_beta_2_4.xml", package = "beautier"))


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

test_that("Reproduce birth_rate_laplace_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_laplace_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_laplace_2_4.xml", package = "beautier"))


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

test_that("Reproduce birth_rate_inv_gamma_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_inv_gamma_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_inv_gamma_2_4.xml", package = "beautier"))


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

test_that("Reproduce birth_rate_poisson_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_poisson_distribution()
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_poisson_2_4.xml", package = "beautier"))


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
# Initial phylogenies
################################################################################

test_that("Reproduce anthus_nd2_anthus_aco_2_4.xml", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    misc_options = beautier::create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "anthus_nd2_anthus_aco_2_4.xml", package = "beautier"))

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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("Reproduce anthus_aco_anthus_nd2_2_4.xml", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "anthus_aco_anthus_nd2_2_4.xml", package = "beautier"))

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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})


test_that("Reproduce aco_hky_nd2.xml", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")

  site_models <- list(
    beautier::create_hky_site_model(),
    beautier::create_jc69_site_model()
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    site_models = site_models,
    misc_options = beautier::create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "aco_hky_nd2.xml", package = "beautier"))

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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("Reproduce aco_nd2_hky.xml", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    site_models = list(create_jc69_site_model(), create_hky_site_model()),
    tree_priors = list(
      create_yule_tree_prior(birth_rate_distribution = create_uniform_distr(id = 1)),
      create_yule_tree_prior(birth_rate_distribution = create_uniform_distr(id = 4))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_hky.xml", package = "beautier"))

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

test_that("Reproduce aco_hky_nd2_tn93.xml, example 9", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    site_models = list(create_hky_site_model(), create_tn93_site_model()),
    tree_priors = create_yule_tree_prior(
      birth_rate_distribution = create_uniform_distr(id = 1)),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_hky_nd2_tn93.xml", package = "beautier"))

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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("Reproduce aco_strict_nd2_rln.xml, example 10", {

  skip("WIP")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    clock_models = list(create_strict_clock_model(), create_rln_clock_model()),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_strict_nd2_rln.xml", package = "beautier"))

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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})
