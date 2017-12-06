context(
  paste(
    "create_beast2_input by reproducing files,",
    "simple and single alignments"
  )
)

################################################################################
# Reproduce files
################################################################################

test_that("2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(beautier:::get_path(
    "2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Site models
################################################################################

################################################################################
# Site model: GTR
################################################################################


test_that("gtr_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "gtr_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created_lines, expected_lines,
      section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("gtr_gcc_1_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "gtr_gcc_1_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines)
  )
})

test_that("gtr_gcc_2_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "gtr_gcc_2_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  skip("WIP: operators section fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )


  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("gtr_gcc_2_shape_1_5_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "gtr_gcc_2_shape_1_5_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  skip("WIP: state section fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )

  skip("WIP: distribution fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  skip("WIP: state section fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Site model: HKY
################################################################################

test_that("hky_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "hky_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})



test_that("hky_kappa_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "hky_kappa_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("hky_prop_invariant_0_5_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "hky_prop_invariant_0_5_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("hky_gcc_1_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "hky_gcc_1_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("hky_gcc_2_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "hky_gcc_2_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created_lines, expected_lines,
      section = "state")
  )

  skip("WIP: distribution fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("hky_gcc_4_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "hky_gcc_4_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created_lines, expected_lines,
      section = "state")
  )

  skip("WIP: distribution fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Site model: JC69
################################################################################

test_that("jc69_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "jc69_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("jc69_gcc_2_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "jc69_gcc_2_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("jc69_gcc_2_shape_1_5_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "jc69_gcc_2_shape_1_5_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", {

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
  expected_lines <- readLines(beautier:::get_path(
    "jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Site model: TN93
################################################################################

test_that("tn93_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "tn93_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("tn93_gcc_1_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "tn93_gcc_1_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("tn93_gcc_2_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "tn93_gcc_2_2_4.xml"))

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(created_lines, expected_lines,
      section = "state")
  )

  skip("WIP: distribution fails")

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})


################################################################################
# Tree priors
################################################################################

################################################################################
# Tree prior: BD
################################################################################

test_that("bd_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "bd_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("bd_6_taxa_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "bd_6_taxa_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("cbs_6_taxa_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_6.fas", package = "beautier"
    ),
    tree_priors = beautier::create_cbs_tree_prior()
  )

  expected_lines <- readLines(beautier:::get_path(
    "cbs_6_taxa_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("ccp_6_taxa_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = system.file(
      "extdata", "test_output_6.fas", package = "beautier"
    ),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected_lines <- readLines(beautier:::get_path(
    "ccp_6_taxa_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("cep_6_taxa_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "cep_6_taxa_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Tree prior: CBS
################################################################################

test_that("cbs_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_cbs_tree_prior()
  )

  expected_lines <- readLines(beautier:::get_path(
    "cbs_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("cbs_2_4.xml is invalid", {

  # cbs_2_4.xml is invalid,
  # because the groupSize's dimension is 5 by default,
  # where the supplied number of taxa is 5. 5 taxa, this 4 nodes, so
  # groupSize cannot be more than 4
  filename <- beautier:::get_path("cbs_2_4.xml")
  testthat::expect_false(is_beast2_input_file(filename))
})

################################################################################
# Tree prior: CCP
################################################################################

test_that("ccp_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected_lines <- readLines(beautier:::get_path(
    "ccp_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("ccp_pop_size_gamma_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "ccp_pop_size_gamma_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operator section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Tree prior: CEP
################################################################################

test_that("cep_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path("cep_2_4.xml"))

  skip("WIP: state section fails")

  testthat::expect_true(
    beautier:::are_equivalent_xml_lines(
      created_lines, expected_lines, section = "state")
  )

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

################################################################################
# Tree prior: Yule
################################################################################

test_that("yule_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(beautier:::get_path(
    "yule_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operatprs section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

################################################################################
# Priors
################################################################################
test_that("birth_rate_uniform_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_uniform_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_normal_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_normal_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_one_div_x_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_one_div_x_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})

test_that("birth_rate_log_normal_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_log_normal_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_exp_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_exp_distr(
        id = 1,
        mean = create_mean_param(id = 5, estimate = FALSE, value = "1.0")
      )
    )
  )

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_exp_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})


test_that("birth_rate_gamma_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_gamma_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_beta_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_beta_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_laplace_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_laplace_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_inv_gamma_2_4.xml", {

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

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_inv_gamma_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)

})

test_that("birth_rate_poisson_2_4.xml", {

  created_lines <- beautier::create_beast2_input(
    input_fasta_filenames = beautier::get_input_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_poisson_distr(
        id = 0,
        lambda = create_lambda_param(id = 14, value = "0.693")
      )
    )
  )

  expected_lines <- readLines(beautier:::get_path(
    "birth_rate_poisson_2_4.xml"))

  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "state")
  )
  testthat::expect_true(
    are_equivalent_xml_lines(created_lines, expected_lines, section = "distribution")
  )

  skip("WIP: operators section fails")

  beautier:::compare_lines(created_lines, expected_lines)
  testthat::expect_identical(created_lines, expected_lines)
})
