context("create_beast2_input")

################################################################################
# General
################################################################################
test_that("input is checked, one alignment", {

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
})

test_that("input is checked, two alignments", {

  fasta_filename_1 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(input_fasta_filenames)

  # Two filesnames, one site model
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = input_fasta_filenames,
      site_models = create_jc69_site_models(ids = "only_one")
    )
  )

  # Two filesnames, one clock model
  if (1 == 2) {
    testthat::expect_silent(
      create_beast2_input(
        input_fasta_filenames = input_fasta_filenames,
        clock_models = create_strict_clock_models(ids = ids[1])
      )
    )
  }

  # Two filesnames, one tree prior
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = input_fasta_filenames,
      tree_priors = create_yule_tree_priors(ids = ids[1])
    )
  )

  # Two filesnames, one phylogeny
  testthat::expect_error(
    create_beast2_input(
      input_fasta_filenames = input_fasta_filenames,
      initial_phylogenies = c(ape::rcoal(4))
    )
  )

})

################################################################################
# Reproduce files
################################################################################

test_that("Reproduce 2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "2_4.xml", package = "beautier"))

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
    input_fasta_filenames = get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_input_fasta_filename()),
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

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_2_2_4.xml"), {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_2_shape_1_5_2_4.xml"), {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that(paste0("Reproduce gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"), {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Site model: HKY
################################################################################

test_that("Reproduce hky_2_4.xml", {


  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})



test_that("Reproduce hky_kappa_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      id = get_id(get_input_fasta_filename()),
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

test_that("Reproduce hky_prop_invariant_0_5_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      id = get_id(get_input_fasta_filename()),
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

test_that("Reproduce hky_gcc_1_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce hky_gcc_2_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce hky_gcc_4_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_hky_site_model(
      id = get_id(get_input_fasta_filename()),
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

test_that("Reproduce jc69_2_4.xml", {

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    site_models = create_jc69_site_model(
      id = id
    ),
    tree_priors = create_yule_tree_prior(
      id = id,
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(system.file("extdata",
    "jc69_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce jc69_gcc_2_2_4.xml", {

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    site_models = create_jc69_site_model(
      id = id,
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected_lines <- readLines(system.file("extdata",
    "jc69_gcc_2_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)

})

test_that("Check that jc69_gcc_2_shape_1_5_2_4.xml is reproduced", {

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    site_models = create_jc69_site_model(
      id = id,
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

  input_fasta_filename <- beautier::get_input_fasta_filename()
  id <- get_id(input_fasta_filename)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filename,
    site_models = create_jc69_site_model(
      id = id,
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
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that tn93_gcc_1_2_4.xml is reproduced", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_tn93_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce tn93_gcc_2_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    site_models = create_tn93_site_model(
      id = get_id(get_input_fasta_filename()),
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

  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Clock models
################################################################################

################################################################################
# Clock model: RLN
################################################################################

test_that("Reproduce rln_2_4.xml", {

  skip("WIP")

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
    "rln_2_4.xml", package = "beautier"))
  testthat::expect_true(are_equivalent_xml_lines(created_lines, expected_lines))
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("Reproduce rln_uclstdev_beta_2_4.xml", {

  skip("WIP")

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
    "rln_uclstdev_beta_2_4.xml", package = "beautier"))

  testthat::expect_true(are_equivalent_xml_lines(created_lines, expected_lines))
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

test_that("Reproduce bd_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_0.fas", package = "beautier"
    ),
    tree_priors = beautier::create_bd_tree_prior(
      birth_rate_distr = beautier::create_uniform_distr(
        id = 3, upper = "1000.0"),
      death_rate_distr = beautier::create_uniform_distr(
        id = 4, upper = NA)
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "bd_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce bd_6_taxa_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_6.fas", package = "beautier"
    ),
    tree_priors = beautier::create_bd_tree_prior(
      birth_rate_distr = beautier::create_uniform_distr(
        id = 3, upper = "1000.0"),
      death_rate_distr = beautier::create_uniform_distr(
        id = 4, upper = NA)
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "bd_6_taxa_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce cbs_6_taxa_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_6.fas", package = "beautier"
    ),
    tree_priors = beautier::create_cbs_tree_prior()
  )

  expected_lines <- readLines(system.file("extdata",
    "cbs_6_taxa_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce ccp_6_taxa_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_6.fas", package = "beautier"
    ),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "ccp_6_taxa_2_4.xml", package = "beautier"))
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce cep_6_taxa_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_6.fas", package = "beautier"
    ),
    tree_priors = beautier::create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 2),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "cep_6_taxa_2_4.xml", package = "beautier"))

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

test_that("Reproduce cbs_2_4.xml", {

  skip("WIP")
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_cbs_tree_prior()
  )

  expected_lines <- readLines(system.file("extdata",
    "cbs_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Check that cbs_2_4.xml is invalid", {

  # cbs_2_4.xml is invalid,
  # because the groupSize's dimension is 5 by default,
  # where the supplied number of taxa is 5. 5 taxa, this 4 nodes, so
  # groupSize cannot be more than 4
  filename <- system.file("extdata",
    "cbs_2_4.xml", package = "beautier")
  testthat::expect_false(is_beast2_input_file(filename))
})

################################################################################
# Tree prior: CCP
################################################################################

test_that("Reproduce ccp_2_4.xml", {

  skip("WIP")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "ccp_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce ccp_pop_size_gamma_2_4.xml", {

  skip("WIP")

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

  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Tree prior: CEP
################################################################################

test_that("Reproduce cep_2_4.xml", {

  skip("WIP")

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
    "cep_2_4.xml", package = "beautier"))

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

  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Initial phylogenies
################################################################################

test_that("Reproduce anthus_nd2_anthus_aco_2_4.xml", {

  skip("WIP")

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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("Reproduce aco_nd2_2_4.xml", {

  skip("WIP")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(input_fasta_filenames)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    tree_priors = list(
      create_yule_tree_prior(
        id = ids[1],
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        id = ids[2],
        birth_rate_distr = create_uniform_distr(id = 4))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_2_4.xml", package = "beautier"))
  testthat::expect_true(are_equivalent_xml_lines(created_lines, expected_lines))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce aco_nd2_nd3_2_4.xml", {

  skip("WIP")
  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_3 <- system.file("extdata",
    "anthus_nd3.fas", package = "beautier")
  input_fasta_filenames <- c(
    fasta_filename_1, fasta_filename_2, fasta_filename_3
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 111)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 222)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 333))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_nd3_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce aco_nd2_nd3_nd4_2_4.xml", {

  skip("WIP")
  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_3 <- system.file("extdata",
    "anthus_nd3.fas", package = "beautier")
  fasta_filename_4 <- system.file("extdata",
    "anthus_nd4.fas", package = "beautier")
  input_fasta_filenames <- c(
    fasta_filename_1, fasta_filename_2, fasta_filename_3, fasta_filename_4
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 111)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 222)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 333)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 444))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_nd3_nd4_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce aco_nd2_nd3_nd4_shared_clock_2_4.xml", {

  skip("WIP")
  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_3 <- system.file("extdata",
    "anthus_nd3.fas", package = "beautier")
  fasta_filename_4 <- system.file("extdata",
    "anthus_nd4.fas", package = "beautier")
  input_fasta_filenames <- c(
    fasta_filename_1, fasta_filename_2, fasta_filename_3, fasta_filename_4
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    clock_models = create_strict_clock_model(get_id(input_fasta_filenames[1])),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 111)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 222)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 333)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 444))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_nd3_nd4_shared_clock_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce aco_nd2_nd3_nd4_complex_2_4.xml", {

  skip("WIP")
  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  fasta_filename_3 <- system.file("extdata",
    "anthus_nd3.fas", package = "beautier")
  fasta_filename_4 <- system.file("extdata",
    "anthus_nd4.fas", package = "beautier")
  input_fasta_filenames <- c(
    fasta_filename_1, fasta_filename_2, fasta_filename_3, fasta_filename_4
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(
      create_jc69_site_model(
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 4,
          gamma_shape = 0.1,
          gamma_shape_prior_distr = create_gamma_distr(
            id = 32,
            alpha = create_alpha_param(id = 174, value = "2.0"),
            beta = create_beta_param(id = 175, value = "2.0")
          ),
          prop_invariant = 0.7
        )
      ),
      create_hky_site_model(
        kappa = "2.1",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 3,
          gamma_shape = 0.2,
          gamma_shape_prior_distr = create_beta_distr(
            id = 4,
            alpha = create_alpha_param(id = 178, value = "2.0"),
            beta = create_beta_param(id = 179, value = "2.0")
          ),
          prop_invariant = 0.6,
          freq_equilibrium = "empirical"
        ),
        kappa_prior_distr = create_poisson_distr(
          id = 8,
          lambda = create_lambda_param(id = 184, value = "0.693")
        )
      ),
      create_tn93_site_model(
        kappa_1_param = create_kappa_1_param(value = "2.2"),
        kappa_2_param = create_kappa_2_param(value = "2.3"),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2,
          gamma_shape = 0.4,
          gamma_shape_prior_distr = create_gamma_distr(
            id = 34,
            alpha = create_alpha_param(id = 180, value = "2.0"),
            beta = create_beta_param(id = 181, value = "2.0")
          ),
          prop_invariant = 0.6
        ),
        kappa_1_prior_distr = create_exp_distr(
          id = 9,
          mean = create_mean_param(id = 185, value = "1.0")
        ),
        kappa_2_prior_distr = create_gamma_distr(
          id = 35,
          alpha = create_alpha_param(id = 186, value = "2.0"),
          beta = create_beta_param(id = 187, value = "2.0")
        ),
        freq_equilibrium = "empirical"
      ),
      create_gtr_site_model(
        rate_ac_param = create_rate_ac_param(value = "1.1"),
        rate_ag_param = create_rate_ag_param(value = "1.2"),
        rate_at_param = create_rate_at_param(value = "1.3"),
        rate_cg_param = create_rate_cg_param(value = "1.4"),
        rate_ct_param = create_rate_ct_param(value = "1.5", estimate = FALSE),
        rate_gt_param = create_rate_gt_param(value = "1.6"),
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 1,
          gamma_shape = 0.8,
          prop_invariant = 0.4
        ),
        rate_ac_prior_distr = create_beta_distr(
          id = 5,
          alpha = create_alpha_param(id = 190, value = "2.0"),
          beta = create_beta_param(id = 191, value = "2.0")
        ),
        rate_ag_prior_distr = create_laplace_distr(
          id = 8,
          mu = create_mu_param(id = 192, value = "0.0"),
          scale = create_scale_param(id = 193, value = "1.0")
        ),
        rate_at_prior_distr = create_inv_gamma_distr(
          id = 5,
          alpha = create_alpha_param(id = 194, value = "2.0"),
          beta = create_beta_param(id = 195, value = "2.0")
        ),
        rate_cg_prior_distr = create_poisson_distr(
          id = 9,
          lambda = create_lambda_param(id = 196, value = "0.693")
        ),
        rate_gt_prior_distr = create_uniform_distr(
          id = 40
        )
      )
    ),
    clock_models = list(
      create_strict_clock_model(
        clock_rate_param = create_clock_rate_param(value = 1.1)
      ),
      create_rln_clock_model(
        mean_clock_rate = 1.1,
        n_rate_categories = 0,
        mean_rate_prior_distr = create_normal_distr(
          id = 5,
          mean = create_mean_param(id = 197, value = "0.0"),
          sigma = create_sigma_param(id = 198, value = "1.0")
        ),
        ucldstdev_distr = create_exp_distr(
          id = 10,
          mean = create_mean_param(id = 203, value = "1.0")
        ),
        mparam_id = 78
      ),
      create_rln_clock_model(
        mean_clock_rate = 1.2,
        n_rate_categories = 1,
        normalize_mean_clock_rate = TRUE,
        mean_rate_prior_distr = create_one_div_x_distr(id = 17),
        ucldstdev_distr = create_laplace_distr(
          id = 9,
          mu = create_mu_param(id = 204, value = "0.0"),
          scale = create_scale_param(id = 205, value = "1.0")
        ),
        mparam_id = 115
      ),
      create_rln_clock_model(
        mean_clock_rate = 1.3,
        n_rate_categories = 2,
        mean_rate_prior_distr = create_log_normal_distr(
          id = 17,
          m = create_m_param(id = 199, value = "1.0"),
          s = create_s_param(
            id = 200, value = "1.25", lower = "0.0", upper = "5.0"
          )
        ),
        ucldstdev_distr = create_inv_gamma_distr(
          id = 6,
          alpha = create_alpha_param(id = 206, value = "2.0"),
          beta = create_beta_param(id = 207, value = "2.0")
        ),
        mparam_id = 147
      )
    ),
    tree_priors = list(
      create_bd_tree_prior(
        birth_rate_distr = create_normal_distr(
          id = 4,
          mean = create_mean_param(id = 169, value = "0.0"),
          sigma = create_sigma_param(id = 170, value = "1.0")
        ),
        death_rate_distr = create_one_div_x_distr(
          id = 16
        )
      ),
      create_ccp_tree_prior(
        pop_size_distr = create_gamma_distr(
          id = 36,
          alpha = create_alpha_param(id = 188, value = "2.0"),
          beta = create_beta_param(id = 189, value = "2.0")
        )
      ),
      create_cep_tree_prior(
        pop_size_distr = create_exp_distr(
          id = 8,
          mean = create_mean_param(id = 173, value = "1.0")
        ),
        growth_rate_distr = create_inv_gamma_distr(
          id = 4,
          alpha = create_alpha_param(id = 182, value = "2.0"),
          beta = create_beta_param(id = 183, value = "2.0")
        )
      ),
      create_yule_tree_prior(
        birth_rate_distr = create_log_normal_distr(
          id = 16,
          m = create_m_param(id = 171, value = "1.0"),
          s = create_s_param(
            id = 172, value = "1.25", lower = "0.0", upper = "5.0"
          )
        )
      )
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )

  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_nd3_nd4_complex_modified_2_4.xml", package = "beautier"))
  testit::assert(are_beast2_input_lines(expected_lines))
  beautier:::compare_lines(created_lines, expected_lines)

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce aco_nd2_shared_site_model_2_4.xml", {

  skip("WIP")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(fasta_filename_1, fasta_filename_2),
    site_models = create_jc69_site_model(id = get_id(fasta_filename_1)),
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
    "aco_nd2_shared_site_model_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("Reproduce aco_hky_nd2.xml", {

  skip("WIP")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(input_fasta_filenames)
  site_models <- list(
    beautier::create_hky_site_model(ids[1]),
    beautier::create_jc69_site_model(ids[2])
  )

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("Reproduce aco_nd2_hky.xml", {

  skip("WIP")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(input_fasta_filenames)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(
      create_jc69_site_model(
        id = ids[1]
      ),
      create_hky_site_model(
        id = ids[2],
        kappa_prior_distr = create_log_normal_distr(
          id = 1,
          m = create_m_param(id = 4, value = "1.0"),
          s = create_s_param(id = 5, value = "1.25", lower = NA, upper = NA)
        )
      )
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = ids[1],
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        id = ids[2],
        birth_rate_distr = create_uniform_distr(id = 4))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_hky.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})


test_that("Reproduce aco_hky_nd2_tn93.xml, example 9", {

  skip("WIP")

  fasta_filename_1 <- system.file("extdata",
    "anthus_aco.fas", package = "beautier")
  fasta_filename_2 <- system.file("extdata",
    "anthus_nd2.fas", package = "beautier")
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(input_fasta_filenames)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(
      create_hky_site_model(ids[1]),
      create_tn93_site_model(ids[2])
    ),
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
  input_fasta_filenames <- c(fasta_filename_1, fasta_filename_2)
  ids <- get_ids(input_fasta_filenames)

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
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

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})




test_that("Reproduce bd_birth_rate_normal_death_rate_gamma_2_4.xml", {

  skip("WIP")

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
    "bd_birth_rate_normal_death_rate_gamma_2_4.xml",
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



test_that("Reproduce aco_nd2_jc69_jc69_strict_rln_yule_yule_2_4", {

  skip("WIP")

  input_fasta_filename_1 <- system.file(
    "extdata", "anthus_aco.fas", package = "beautier"
  )
  input_fasta_filename_2 <- system.file(
    "extdata", "anthus_nd2.fas", package = "beautier"
  )
  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = c(input_fasta_filename_1, input_fasta_filename_2),
    site_models = list(
      create_jc69_site_model(

      ),
      create_jc69_site_model(
      )
    ),
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
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4)
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_jc69_jc69_strict_rln_yule_yule_2_4.xml", package = "beautier"))

  testthat::expect_identical(created_lines, expected_lines)
})


test_that("JC69 JC69 strict relaxed_log_normal yule", {

  skip("WIP")

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
  clock_model_2 <- create_rln_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_fasta_filenames = input_fasta_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(has_unique_ids(lines))

  if (!is_on_travis()) return()
  save_text("~/fix.xml", lines)
  are_beast2_input_lines(lines)
})



#-------------------------------------------------------------------------------
# Brute force tests, two alignments
#-------------------------------------------------------------------------------
test_that("All site models, clock models and tree priors, crown age est", {

  if (!is_on_travis()) return()

  skip("WIP")

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
