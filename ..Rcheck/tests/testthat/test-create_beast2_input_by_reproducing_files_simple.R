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

  created <- beautier::create_beast2_input(
    input_filenames = get_fasta_filename(),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected <- readLines(beautier::get_beautier_path("2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Site models
################################################################################

################################################################################
# Site model: GTR
################################################################################


test_that("gtr_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = get_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_fasta_filename()),
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

  expected <- readLines(beautier::get_beautier_path("gtr_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_1_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_gtr_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path("gtr_gcc_1_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_2_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_gtr_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path("gtr_gcc_2_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_2_shape_1_5_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_gtr_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "gtr_gcc_2_shape_1_5_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_gtr_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("gtr_no_rate_estimation_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = get_fasta_filename(),
    site_models = create_gtr_site_model(
      id = get_id(get_fasta_filename()),
      rate_ac_param = create_rate_ac_param(value = "1.0", estimate = FALSE),
      rate_ag_param = create_rate_ag_param(value = "1.0", estimate = FALSE),
      rate_at_param = create_rate_at_param(value = "1.0", estimate = FALSE),
      rate_cg_param = create_rate_cg_param(value = "1.0", estimate = FALSE),
      rate_ct_param = create_rate_ct_param(value = "1.0", estimate = FALSE),
      rate_gt_param = create_rate_gt_param(value = "1.0", estimate = FALSE)
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected <- readLines(beautier::get_beautier_path(
    "gtr_no_rate_estimation_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


################################################################################
# Site model: HKY
################################################################################

test_that("hky_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_hky_site_model(
      id = get_id(get_fasta_filename()),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected <- readLines(beautier::get_beautier_path("hky_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})



test_that("hky_kappa_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_hky_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "hky_kappa_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("hky_prop_invariant_0_5_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_hky_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "hky_prop_invariant_0_5_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("hky_gcc_1_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_hky_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "hky_gcc_1_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("hky_gcc_2_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_hky_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "hky_gcc_2_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("hky_gcc_4_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_hky_site_model(
      id = get_id(get_fasta_filename()),
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
  expected <- readLines(beautier::get_beautier_path(
    "hky_gcc_4_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Site model: JC69
################################################################################

test_that("jc69_2_4.xml", {

  input_fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_id(input_fasta_filename)
  created <- beautier::create_beast2_input(
    input_filenames = input_fasta_filename,
    site_models = create_jc69_site_model(
      id = id
    ),
    tree_priors = create_yule_tree_prior(
      id = id,
      birth_rate_distr = create_uniform_distr(id = 1))
  )

  expected <- readLines(beautier::get_beautier_path("jc69_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("jc69_gcc_2_2_4.xml", {

  input_fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_id(input_fasta_filename)

  created <- beautier::create_beast2_input(
    input_filenames = input_fasta_filename,
    site_models = create_jc69_site_model(
      id = id,
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected <- readLines(beautier::get_beautier_path(
    "jc69_gcc_2_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("jc69_gcc_2_shape_1_5_2_4.xml", {

  input_fasta_filename <- beautier::get_beautier_path("test_output_0.fas")
  id <- get_id(input_fasta_filename)

  created <- beautier::create_beast2_input(
    input_filenames = input_fasta_filename,
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
  expected <- readLines(beautier::get_beautier_path(
    "jc69_gcc_2_shape_1_5_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_jc69_site_model(
      id = "test_output_0",
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5,
        prop_invariant = 0.5
      )
    ),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))

  )
  expected <- readLines(beautier::get_beautier_path(
    "jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Site model: TN93
################################################################################

test_that("tn93_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_tn93_site_model(
      id = get_id(get_fasta_filename()),
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

  expected <- readLines(beautier::get_beautier_path(
    "tn93_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("tn93_gcc_1_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_tn93_site_model(
      id = get_id(get_fasta_filename()),
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

  expected <- readLines(beautier::get_beautier_path(
    "tn93_gcc_1_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("tn93_gcc_2_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    site_models = create_tn93_site_model(
      id = get_id(get_fasta_filename()),
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

  expected <- readLines(beautier::get_beautier_path(
    "tn93_gcc_2_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


################################################################################
# Tree priors
################################################################################

################################################################################
# Tree prior: BD
################################################################################

test_that("bd_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = beautier::create_bd_tree_prior(
      birth_rate_distr = beautier::create_uniform_distr(
        id = 3, upper = "1000.0"),
      death_rate_distr = beautier::create_uniform_distr(
        id = 4, upper = NA)
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "bd_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("bd_6_taxa_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_6.fas"),
    tree_priors = beautier::create_bd_tree_prior(
      birth_rate_distr = beautier::create_uniform_distr(
        id = 3, upper = "1000.0"),
      death_rate_distr = beautier::create_uniform_distr(
        id = 4, upper = NA)
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "bd_6_taxa_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("cbs_6_taxa_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_6.fas"),
    tree_priors = beautier::create_cbs_tree_prior()
  )
  expected <- readLines(beautier::get_beautier_path(
    "cbs_6_taxa_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("ccp_6_taxa_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_6.fas"),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "ccp_6_taxa_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("cep_6_taxa_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_6.fas"),
    tree_priors = beautier::create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 2),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "cep_6_taxa_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: CBS
################################################################################

test_that("cbs_2_4.xml", {

  testthat::expect_error(
    beautier::create_beast2_input(
      input_filenames = beautier::get_beautier_path("test_output_0.fas"),
      tree_priors = create_cbs_tree_prior()
    ),
    "'group_sizes_dimension' \\(5\\) must be less than the number of taxa \\(5\\)" # nolint
  )
})

test_that("cbs_6_taxa_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_6.fas"),
    tree_priors = create_cbs_tree_prior()
  )
  expected <- readLines(beautier::get_beautier_path(
    "cbs_6_taxa_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_cbs_groupsize_4_dim.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("anthus_aco_sub.fas"),
    tree_priors = create_cbs_tree_prior(
      group_sizes_dimension = 4
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )
  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub_cbs_groupsize_4_dim.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})



################################################################################
# Tree prior: CCP
################################################################################

test_that("ccp_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1)
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "ccp_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("ccp_pop_size_gamma_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = beautier::create_ccp_tree_prior(
      pop_size_distr = beautier::create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 9, value = "2.0"),
        beta = create_beta_param(id = 10, value = "2.0")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "ccp_pop_size_gamma_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: CEP
################################################################################

test_that("cep_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = beautier::create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path("cep_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: Yule
################################################################################

test_that("yule_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected <- readLines(beautier::get_beautier_path("yule_2_4.xml"))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Prior distributions
################################################################################
test_that("birth_rate_uniform_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1))
  )
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_uniform_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_normal_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_normal_distr(
        id = 0,
        mean = create_mean_param(id = 1, estimate = FALSE, value = "0.0"),
        sigma = create_sigma_param(id = 2, estimate = FALSE, value = "1.0")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_normal_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_one_div_x_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_one_div_x_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_log_normal_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
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
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_log_normal_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_exp_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_exp_distr(
        id = 1,
        mean = create_mean_param(id = 5, estimate = FALSE, value = "1.0")
      )
    )
  )

  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_exp_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


test_that("birth_rate_gamma_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = beautier::create_yule_tree_prior(
      birth_rate_distr = beautier::create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 6, value = "2.0"),
        beta = create_beta_param(id = 7, value = "2.0")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_gamma_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_beta_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_beta_distr(
        id = 0,
        alpha = create_alpha_param(id = 8, value = "2.0"),
        beta = create_beta_param(id = 9, value = "2.0")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_beta_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_laplace_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 10, value = "0.0"),
        scale = create_scale_param(id = 11, value = "1.0")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_laplace_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_inv_gamma_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
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
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_inv_gamma_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_poisson_2_4.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("test_output_0.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_poisson_distr(
        id = 0,
        lambda = create_lambda_param(id = 14, value = "0.693")
      )
    )
  )
  expected <- readLines(beautier::get_beautier_path(
    "birth_rate_poisson_2_4.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# MCMC options
################################################################################

test_that("aco_mcmc_chainlength_10_store_every_1.xml", {

  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("anthus_aco.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length =  10, store_every = 1),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "aco_mcmc_chainlength_10_store_every_1.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# MRCA priors
################################################################################
# No calibration yet
test_that("anthus_aco_sub.xml", {
  created <- beautier::create_beast2_input(
    input_filenames = beautier::get_beautier_path("anthus_aco_sub.fas"),
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibration.xml", {

  # This XML file has a normal distribution for the MRCA prior
  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_priors = create_mrca_prior(
      name = "all",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      mrca_distr = create_normal_distr(
        id = 0,
        mean = create_mean_param(id = 1, value = "0.02"),
        sigma = create_sigma_param(id = 2, value = "0.001")
      ),
      is_monophyletic = TRUE,
      clock_prior_distr_id = 0
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub_calibrated.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibrated_no_prior.xml", {
  # This XML file has no prior distribution

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_priors = create_mrca_prior(
      name = "every",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      clock_prior_distr_id = 0
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub_calibrated_no_prior.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibrated_rln.xml", {

  # This XML file has
  # - an MRCA prior distribution prior,
  # - an RLN clock model

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 6,
        alpha = create_alpha_param(id = 21, value = "0.5396"),
        beta = create_beta_param(id = 22, value = "0.3819")
      ),
      mparam_id = 20
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_priors = create_mrca_prior(
      name = "every",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      clock_prior_distr_id = 0
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )
  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub_calibrated_rln.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibrated_rln_monophyletic.xml", {

  # This XML file has
  # - an MRCA prior distribution prior,
  # - an RLN clock model, that is monophyletic

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 6,
        alpha = create_alpha_param(id = 21, value = "0.5396"),
        beta = create_beta_param(id = 22, value = "0.3819")
      ),
      mparam_id = 20
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_priors = create_mrca_prior(
      name = "every",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      clock_prior_distr_id = 0,
      is_monophyletic = TRUE
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )
  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub_calibrated_rln_monophyletic.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})


test_that("anthus_aco_sub_two_mrca_priors.xml", {

  fasta_filename <- beautier::get_beautier_path("anthus_aco_sub.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_priors = list(
      create_mrca_prior(
        name = "first_two",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)[1:2],
        clock_prior_distr_id = 0
      ),
      create_mrca_prior(
        name = "last_three",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)[3:5],
        clock_prior_distr_id = 0
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "anthus_aco_sub_two_mrca_priors.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("test_0_two_mrca_priors.xml, #30", {

  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_priors = create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 4, value = "0.001"),
        scale = create_scale_param(id = 5, value = "30.701135")
      )
    ),
    mrca_priors = list(
      create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      ),
      create_mrca_prior(
        name = "one_and_three",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)[c(2, 5)],
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = FALSE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "test_output_0_two_mrca_priors.xml")
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("issue_30.xml, #30", {

  fasta_filename <- beautier::get_beautier_path("test_output_0.fas")

  created <- beautier::create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mrca_priors = list(
      create_mrca_prior(
        name = "most",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = c(paste0("t", seq(1, 4))),
        is_monophyletic = FALSE,
        clock_prior_distr_id = 0
      ),
      create_mrca_prior(
        name = "some_mono",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = c(paste0("t", seq(2, 3))),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = FALSE)
  )

  expected <- readLines(beautier::get_beautier_path(
    "issue_30.xml")
  )
  beautier:::compare_lines(created, expected)
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
