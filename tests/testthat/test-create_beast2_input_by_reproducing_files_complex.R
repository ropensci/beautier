context(
  paste(
    "create_beast2_input by reproducing files,",
    "multiple and complex alignments"
  )
)

################################################################################
# Two alignments
################################################################################

test_that("anthus_nd2_anthus_aco_2_4.xml", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
  testthat::expect_true(are_equivalent_xml_lines(created_lines, expected_lines))

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("aco_nd2_2_4.xml", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )


  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
  testthat::expect_true(are_equivalent_xml_lines(created_lines, expected_lines))

  testthat::expect_identical(created_lines, expected_lines)
})

test_that("aco_nd2_nd3_2_4.xml", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("aco_nd2_nd3_nd4_2_4.xml", {

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
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 7)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 10))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected_lines <- readLines(system.file("extdata",
    "aco_nd2_nd3_nd4_2_4.xml", package = "beautier"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})


test_that("aco_nd2_shared_site_model_2_4.xml", {

  skip("WIP: tracelog")

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

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  skip("WIP: state section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("aco_hky_nd2.xml", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})

test_that("aco_nd2_hky.xml", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

  testthat::expect_identical(created_lines, expected_lines)
})


test_that("aco_hky_nd2_tn93.xml, example 9", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

  if (is_on_travis()) {
    testthat::expect_true(beautier::are_beast2_input_lines(created_lines))
  } else {
    if (1 == 2) {
      testthat::expect_identical(created_lines, expected_lines)
    }
  }
})


test_that("bd_birth_rate_normal_death_rate_gamma_2_4.xml", {

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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operator section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("aco_nd2_jc69_jc69_strict_rln_yule_yule_2_4", {

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
        ucldstdev_distr = create_gamma_distr(
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

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution section fails")

  testthat::expect_true(
    are_equal_xml_lines(created_lines, expected_lines, section = "distribution")
  )


  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Three alignments
################################################################################

################################################################################
# Four alignments
################################################################################

test_that("aco_nd2_nd3_nd4_complex_2_4.xml", {

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
        ),
        freq_equilibrium = "all_equal"
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
    "aco_nd2_nd3_nd4_complex_2_4.xml", package = "beautier"))
  testit::assert(are_beast2_input_lines(expected_lines))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})
