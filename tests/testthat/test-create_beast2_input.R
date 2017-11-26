context("create_beast2_input")

################################################################################
# General
################################################################################
test_that("input is checked", {

  testthat::expect_silent(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename()
    )
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = "nonexisting" # Error
    ),
    "input_fasta_filenames not found"
  )
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      mcmc = "nonsense"
    ),
    "mcmc must be a valid mcmc object, as returned by 'create_mcmc'"
  )

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      tree_priors = "nonsense"
    ),
    "tree_priors must be valid, as returned by 'create_tree_priors'"
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

  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = get_input_fasta_filename(),
      initial_phylogenies = "nonsense"
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
      site_models = create_jc69_site_models(n = 1)
    )
  )

  # Two filesnames, one clock model
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      clock_models = create_strict_clock_models(ids = "only_one")
    )
  )

  # Two filesnames, one tree prior
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
      tree_priors = create_yule_tree_priors(n = 1)
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

################################################################################
# Reproduce files
################################################################################

test_that("Reproduce 2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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

test_that("Run all defaults", {

  if (!beautier::is_on_travis()) return()

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename()
  )

  testthat::expect_true(are_beast2_input_lines(created_lines))
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
    site_models = create_gtr_site_model(
      rate_ac_prior_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 7, value = "0.05"),
        beta = create_beta_param(id = 8, value = "10.0")
      ),
      rate_ag_prior_distr = create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 9, value = "0.05"),
        beta = create_beta_param(id = 10, value = "20.0")
      ),
      rate_at_prior_distr = create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 11, value = "0.05"),
        beta = create_beta_param(id = 12, value = "10.0")
      ),
      rate_cg_prior_distr = create_gamma_distr(
        id = 3,
        alpha = create_alpha_param(id = 13, value = "0.05"),
        beta = create_beta_param(id = 14, value = "10.0")
      ),
      rate_gt_prior_distr = create_gamma_distr(
        id = 5,
        alpha = create_alpha_param(id = 17, value = "0.05"),
        beta = create_beta_param(id = 18, value = "10.0")
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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

test_that("Run GTR", {

  if (!beautier::is_on_travis()) return()

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model()
  )

  testthat::expect_true(are_beast2_input_lines(created_lines))
})

test_that(paste0("Reproduce gtr_gcc_1_2_4.xml"), {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 1
      ),
      rate_ac_prior_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 1, value = "0.05"),
        beta = create_beta_param(id = 2, value = "10.0")
      ),
      rate_ag_prior_distr = create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 3, value = "0.05"),
        beta = create_beta_param(id = 4, value = "20.0")
      ),
      rate_at_prior_distr = create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 5, value = "0.05"),
        beta = create_beta_param(id = 6, value = "10.0")
      ),
      rate_cg_prior_distr = create_gamma_distr(
        id = 3,
        alpha = create_alpha_param(id = 7, value = "0.05"),
        beta = create_beta_param(id = 8, value = "10.0")
      ),
      rate_gt_prior_distr = create_gamma_distr(
        id = 5,
        alpha = create_alpha_param(id = 11, value = "0.05"),
        beta = create_beta_param(id = 12, value = "10.0")
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      rate_ac_prior_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 1, value = "0.05"),
        beta = create_beta_param(id = 2, value = "10.0")
      ),
      rate_ag_prior_distr = create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 3, value = "0.05"),
        beta = create_beta_param(id = 4, value = "20.0")
      ),
      rate_at_prior_distr = create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 5, value = "0.05"),
        beta = create_beta_param(id = 6, value = "10.0")
      ),
      rate_cg_prior_distr = create_gamma_distr(
        id = 3,
        alpha = create_alpha_param(id = 7, value = "0.05"),
        beta = create_beta_param(id = 8, value = "10.0")
      ),
      rate_gt_prior_distr = create_gamma_distr(
        id = 5,
        alpha = create_alpha_param(id = 11, value = "0.05"),
        beta = create_beta_param(id = 12, value = "10.0")
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      rate_ac_prior_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 1, value = "0.05"),
        beta = create_beta_param(id = 2, value = "10.0")
      ),
      rate_ag_prior_distr = create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 3, value = "0.05"),
        beta = create_beta_param(id = 4, value = "20.0")
      ),
      rate_at_prior_distr = create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 5, value = "0.05"),
        beta = create_beta_param(id = 6, value = "10.0")
      ),
      rate_cg_prior_distr = create_gamma_distr(
        id = 3,
        alpha = create_alpha_param(id = 7, value = "0.05"),
        beta = create_beta_param(id = 8, value = "10.0")
      ),
      rate_gt_prior_distr = create_gamma_distr(
        id = 5,
        alpha = create_alpha_param(id = 11, value = "0.05"),
        beta = create_beta_param(id = 12, value = "10.0")
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      rate_ac_prior_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 1, value = "0.05"),
        beta = create_beta_param(id = 2, value = "10.0")
      ),
      rate_ag_prior_distr = create_gamma_distr(
        id = 1,
        alpha = create_alpha_param(id = 3, value = "0.05"),
        beta = create_beta_param(id = 4, value = "20.0")
      ),
      rate_at_prior_distr = create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 5, value = "0.05"),
        beta = create_beta_param(id = 6, value = "10.0")
      ),
      rate_cg_prior_distr = create_gamma_distr(
        id = 3,
        alpha = create_alpha_param(id = 7, value = "0.05"),
        beta = create_beta_param(id = 8, value = "10.0")
      ),
      rate_gt_prior_distr = create_gamma_distr(
        id = 5,
        alpha = create_alpha_param(id = 11, value = "0.05"),
        beta = create_beta_param(id = 12, value = "10.0")
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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

test_that("Reproduce hky_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "hky_2_4.xml", package = "beautier"))

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



test_that("Reproduce hky_kappa_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      kappa = 3.4,
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_uniform_distr(id = 1))

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
    site_models = create_tn93_site_model(
      kappa_1_prior_distr = create_log_normal_distr(
        id = 1,
        m = create_m_param(id = 3, value = "1.0"),
        s = create_s_param(id = 4, value = "1.25", lower = NA, upper = NA)
      ),
      kappa_2_prior_distr = create_log_normal_distr(
        id = 2,
        m = create_m_param(id = 5, value = "1.0"),
        s = create_s_param(id = 6, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      gamma_site_model = create_gamma_site_model(gamma_cat_count = 1),
      kappa_1_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      ),
      kappa_2_prior_distr = create_log_normal_distr(
        id = 1,
        m = create_m_param(id = 3, value = "1.0"),
        s = create_s_param(id = 4, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      gamma_site_model = create_gamma_site_model(gamma_cat_count = 2),
      kappa_1_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      ),
      kappa_2_prior_distr = create_log_normal_distr(
        id = 1,
        m = create_m_param(id = 3, value = "1.0"),
        s = create_s_param(id = 4, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
    clock_models = create_rln_clock_model(
      uclstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
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

test_that("Reproduce relaxed_clock_log_normal_uclstdev_beta_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_rln_clock_model(
      uclstdev_distr = create_beta_distr(
        id = 0,
        alpha = create_alpha_param(id = 4, value = "2.0"),
        beta = create_beta_param(id = 5, value = "2.0")
      ),
      mparam_id = 1
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "relaxed_clock_log_normal_uclstdev_beta_2_4.xml", package = "beautier"))


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

test_that("Use of a strict clock", {

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)
  lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    clock_models = create_strict_clock_model(
      clock_rate_param = create_clock_rate_param(id = id)
    )
  )
  testthat::expect_true(has_unique_ids(lines))
})

test_that("Use of a RLN clock", {

  lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    clock_models = create_rln_clock_model()
  )
  testthat::expect_true(has_unique_ids(lines))

})

################################################################################
# Clock model: strict
################################################################################

test_that("Check that strict_clock_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "strict_clock_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce strict_clock_rate_0_5_2_4.xml", {

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    clock_models = create_strict_clock_model(
      clock_rate_param = create_clock_rate_param(
        id = id,
        value = "0.5"
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
    tree_priors = beautier::create_bd_tree_prior(
      birth_rate_distr = beautier::create_uniform_distr(
        id = 3, upper = "1000.0"),
      death_rate_distr = beautier::create_uniform_distr(
        id = 4, upper = NA)
    )
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

test_that("Run BD tree prior", {

  if (!beautier::is_on_travis()) return()

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_bd_tree_prior()
  )

  testthat::expect_true(are_beast2_input_lines(created_lines))
})


################################################################################
# Tree prior: CBS
################################################################################

test_that("Check that coalescent_bayesian_skyline_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_cbs_tree_prior()
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

test_that("Reproduce coalescent_constant_population_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "coalescent_constant_population_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce ccp_pop_size_gamma_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = beautier::create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 9, value = "2.0"),
        beta = create_beta_param(id = 10, value = "2.0")
      )
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "ccp_pop_size_gamma_2_4.xml", package = "beautier"))

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
# Tree prior: CEP
################################################################################

test_that("Reproduce coalescent_exp_population_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "coalescent_exp_population_2_4.xml", package = "beautier"))

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

test_that("Run CEP", {

  lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_cep_tree_prior()
  )
  testthat::expect_true(has_unique_ids(lines))

})

################################################################################
# Tree prior: Yule
################################################################################

test_that("Reproduce yule_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_uniform_distr(id = 1))
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
      birth_rate_distr = create_normal_distr(
        id = 0,
        mean = create_mean_param(id = 1, estimate = FALSE, value = "0.0"),
        sigma = create_sigma_param(id = 2, estimate = FALSE, value = "1.0")
      )
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
      birth_rate_distr = create_one_div_x_distr(id = 1)
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
      birth_rate_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 3, estimate = FALSE, value = "1.0"),
        s = create_s_param(
          id = 4,
          estimate = FALSE,
          value = "1.25",
          lower = "0.0",
          upper = "5.0"
        )
      )
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

test_that("Reproduce birth_rate_exp_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_exp_distr(
        id = 1,
        mean = create_mean_param(id = 5, estimate = FALSE, value = "1.0")
      )
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "birth_rate_exp_2_4.xml", package = "beautier"))


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
      birth_rate_distr = beautier::create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 6, value = "2.0"),
        beta = create_beta_param(id = 7, value = "2.0")
      )
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
      birth_rate_distr = create_beta_distr(
        id = 0,
        alpha = create_alpha_param(id = 8, value = "2.0"),
        beta = create_beta_param(id = 9, value = "2.0")
      )
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
      birth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 10, value = "0.0"),
        scale = create_scale_param(id = 11, value = "1.0")
      )
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
      birth_rate_distr = create_inv_gamma_distr(
        id = 0,
        alpha = create_alpha_param(
          id = 12,
          estimate = FALSE,
          value = "2.0"
        ),
        beta = create_beta_param(
          id = 13,
          estimate = FALSE,
          value = "2.0"
        )
      )
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
      birth_rate_distr = create_poisson_distr(
        id = 0,
        lambda = create_lambda_param(id = 14, value = "0.693")
      )
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
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 2))
    ),
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
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 2))
    ),
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
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 2))
    ),
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
    site_models = list(
      create_jc69_site_model(),
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(
          id = 1,
          m = create_m_param(id = 4, value = "1.0"),
          s = create_s_param(id = 5, value = "1.25", lower = NA, upper = NA)
        )
        )
      ),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4))
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
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 2)
      )
    ),
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
    clock_models = list(
      create_strict_clock_model(),
      create_rln_clock_model(
        uclstdev_distr = create_gamma_distr(
          id = 0,
          alpha = create_alpha_param(id = 3, value = "0.5396"),
          beta = create_beta_param(id = 4, value = "0.3819")
        )
      )
    ),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4))
    ),
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




test_that("Reproduce birth_death_birth_rate_normal_death_rate_gamma_2_4.xml", {

  fasta_filename <- system.file("extdata",
    "test_output_0.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = fasta_filename,
    tree_priors = create_bd_tree_prior(
        birth_rate_distr = create_normal_distr(
          id = 0,
          mean = create_mean_param(id = 3, value = "0.0"),
          sigma = create_sigma_param(id = 4, value = "1.0")
        ),
        death_rate_distr = create_gamma_distr(
          id = 1,
          alpha = create_alpha_param(id = 7, value = "2.0"),
          beta = create_beta_param(id = 8, value = "2.0")
        )
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "birth_death_birth_rate_normal_death_rate_gamma_2_4.xml",
    package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("JC69 JC69 strict strict coalescent_exp_population", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_cep_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(has_unique_ids(lines))
})

test_that("TN93 TN93 strict strict yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_tn93_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(has_unique_ids(lines))
})



test_that("GTR GTR strict strict yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_gtr_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(has_unique_ids(lines))
})


test_that("GTR TN93 strict strict yule", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(has_unique_ids(lines))
})

#-------------------------------------------------------------------------------
# Brute force tests, two alignments
#-------------------------------------------------------------------------------
test_that("All site models, clock models and tree priors, crown age est", {

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  input_fasta_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)

  for (site_model_1 in beautier::create_site_models()) {
    for (site_model_2 in beautier::create_site_models()) {
      for (clock_model_1 in beautier::create_clock_models()) {
        for (clock_model_2 in beautier::create_clock_models()) {
          for (tree_prior in beautier::create_tree_priors()) {

            lines <- create_beast2_input(
              input_fasta_filenames = input_fasta_filenames,
              site_models = list(site_model_1, site_model_2),
              clock_models = list(clock_model_1, clock_model_2),
              tree_priors = list(tree_prior, tree_prior)
            )
            testthat::expect_true(has_unique_ids(lines))
          }
        }
      }
    }
  }
})
