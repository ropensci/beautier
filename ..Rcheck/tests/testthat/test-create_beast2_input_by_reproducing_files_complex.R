context(
  paste(
    "create_beast2_input by reproducing files,",
    "multiple and complex alignments"
  )
)

################################################################################
# Two alignments
################################################################################

test_that("aco_nd2_2_4.xml", {
#
  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_paths(
      c("anthus_aco.fas", "anthus_nd2.fas")
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = "anthus_aco",
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        id = "anthus_nd2",
        birth_rate_distr = create_uniform_distr(id = 4))
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected <- readLines(beautier::get_beautier_path("aco_nd2_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "state")
  )

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "distribution")
  )

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "operators")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("aco_nd2_nd3_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_paths(
      c("anthus_aco.fas", "anthus_nd2.fas", "anthus_nd3.fas")
    ),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 7))
    ),
    clock_models = list(
      create_strict_clock_model(),
      create_strict_clock_model(
        clock_rate_distr = create_uniform_distr(id = 3)
      ),
      create_strict_clock_model(
        clock_rate_distr = create_uniform_distr(id = 6)
      )
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected <- readLines(beautier::get_beautier_path("aco_nd2_nd3_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "state")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "distribution")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "operators")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("aco_nd2_nd3_nd4_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_paths(
      c("anthus_aco.fas", "anthus_nd2.fas", "anthus_nd3.fas", "anthus_nd4.fas")
    ),
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
    clock_models = list(
      create_strict_clock_model(),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      ),
      create_strict_clock_model(
        id = "anthus_nd3",
        clock_rate_distr = create_uniform_distr(id = 6)
      ),
      create_strict_clock_model(
        id = "anthus_nd4",
        clock_rate_distr = create_uniform_distr(id = 9)
      )
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected <- readLines(beautier::get_beautier_path("aco_nd2_nd3_nd4_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "state")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "distribution")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "operators")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


test_that("aco_nd2_shared_site_model_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_paths(
      c("anthus_aco.fas", "anthus_nd2.fas")
    ),
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_aco")
    ),
    tree_priors = list(
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)),
      create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 4))
    ),
    clock_models = list(
      create_strict_clock_model(),
      create_strict_clock_model(
        clock_rate_distr = create_uniform_distr(id = 3)
      )
    ),
    misc_options = create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "aco_nd2_shared_site_model_2_4.xml"))

  skip("WIP: state, 2 alignments, shared site model")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "state")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "operators")
  )

  skip("WIP: distribution, 2 alignments, shared site model")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "distribution")
  )
  beautier:::compare_lines(created, expected,
    section = "distribution")
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("aco_hky_nd2.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_paths(
      c("anthus_aco.fas", "anthus_nd2.fas")
    ),
    site_models = list(
      create_hky_site_model(
        kappa_prior_distr = create_log_normal_distr(
          id = 0,
          m = create_m_param(id = 2, value = "1.0"),
          s = create_s_param(id = 3, value = "1.25", lower = NA, upper = NA)
        )
      ),
      create_jc69_site_model()
    ),
    clock_models = list(
      create_strict_clock_model(),
      create_strict_clock_model(
        clock_rate_distr = create_uniform_distr(id = 3)
      )
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = "anthus_aco",
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      create_yule_tree_prior(
        id = "anthus_nd2",
        birth_rate_distr = create_uniform_distr(id = 4)
      )
    ),
    misc_options = beautier::create_misc_options(
      capitalize_first_char_id = FALSE,
      nucleotides_uppercase = TRUE
    )
  )

  expected <- readLines(beautier::get_beautier_path("aco_hky_nd2.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "state")
  )

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "distribution")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "operators")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("bd_birth_rate_normal_death_rate_gamma_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
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
  expected <- readLines(beautier::get_beautier_path(
    "bd_birth_rate_normal_death_rate_gamma_2_4.xml"))



  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "state")
  )

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "state")
  )
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "distribution")
  )

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created, expected,
      section = "operators")
  )

  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Three alignments
################################################################################

################################################################################
# Four alignments
################################################################################

test_that("aco_nd2_nd3_nd4_complex_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_paths(
      c("anthus_aco.fas", "anthus_nd2.fas", "anthus_nd3.fas", "anthus_nd4.fas")
    ),
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
          freq_equilibrium = "estimated"
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
        id = "anthus_aco",
        clock_rate_param = create_clock_rate_param(value = 1.1)
      ),
      create_rln_clock_model(
        id = "anthus_nd2",
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
        id = "anthus_nd3",
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
        id = "anthus_nd4",
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
        id = "anthus_aco",
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
        id = "anthus_nd2",
        pop_size_distr = create_gamma_distr(
          id = 36,
          alpha = create_alpha_param(id = 188, value = "2.0"),
          beta = create_beta_param(id = 189, value = "2.0")
        )
      ),
      create_cep_tree_prior(
        id = "anthus_nd3",
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
        id = "anthus_nd4",
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

  expected <- readLines(beautier::get_beautier_path(
    "aco_nd2_nd3_nd4_complex_2_4.xml"))

  skip("WIP: state, 4 alignments")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "state")
  )
  beautier:::compare_lines(created, expected, section = "state")
  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "distribution")
  )

  skip("WIP: operators, 4 alignments")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created, expected, section = "operators")
  )

  beautier:::compare_lines(created, expected,
    section = "operators")
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
