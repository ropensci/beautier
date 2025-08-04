################################################################################
# Defaults for different versions
################################################################################
test_that("2.4", {

  created <- create_beast2_input(
    input_filename = get_fasta_filename(),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options_v2_4()
  )
  expected <- readLines(get_beautier_path("2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("v2.5", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options(
      beast2_version = "2.5",
      required = "BEAST v2.5.0",
      nucleotides_uppercase = TRUE
    )
  )
  expected <- readLines(get_beautier_path("anthus_aco_sub_20181016.xml"))
  # Creates temporary files in beautier folder
  compare_lines(
    lines = created,
    expected = expected
  )
  expect_true(are_equivalent_xml_lines(created, expected))
  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("v2.5.1", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options(
      beast2_version = "2.5",
      required = "BEAST v2.5.1",
      nucleotides_uppercase = TRUE
    )
  )
  expected <- readLines(get_beautier_path("anthus_aco_sub_2_5_1.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_2_6.xml", {
  created <- create_beast2_input_from_model(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    inference_model = create_inference_model(
      tree_prior = create_yule_tree_prior(
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      beauti_options = create_beauti_options_v2_6(
        nucleotides_uppercase = TRUE,
        namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" # nolint indeed a long line
      )
    )
  )
  expected <- readLines(get_beautier_path("anthus_aco_sub_2_6.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Site models
################################################################################

################################################################################
# Site model: GTR
################################################################################


test_that("gtr_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_fasta_filename(),
    site_model = create_gtr_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("gtr_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_1_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_gtr_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("gtr_gcc_1_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_2_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_gtr_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("gtr_gcc_2_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_2_shape_1_5_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_gtr_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "gtr_gcc_2_shape_1_5_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_gtr_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "gtr_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("gtr_no_rate_estimation_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_fasta_filename(),
    site_model = create_gtr_site_model(
      id = get_alignment_id(get_fasta_filename()),
      rate_ac_param = create_rate_ac_param(value = "1.0", estimate = FALSE),
      rate_ag_param = create_rate_ag_param(value = "1.0", estimate = FALSE),
      rate_at_param = create_rate_at_param(value = "1.0", estimate = FALSE),
      rate_cg_param = create_rate_cg_param(value = "1.0", estimate = FALSE),
      rate_ct_param = create_rate_ct_param(value = "1.0"),
      rate_gt_param = create_rate_gt_param(value = "1.0", estimate = FALSE)
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "gtr_no_rate_estimation_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})


################################################################################
# Site model: HKY
################################################################################

test_that("hky_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_hky_site_model(
      id = get_alignment_id(get_fasta_filename()),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )

  expected <- readLines(get_beautier_path("hky_2_4.xml"))
  # Creates temporary files in beautier folder
  compare_lines(
    lines = created,
    expected = expected
  )
  expect_true(are_equivalent_xml_lines(created, expected))
  remove_beautier_folder()
  check_empty_beautier_folder()
})


test_that("hky_2_6.xml", {
  check_empty_beautier_folder()

  inference_model <- create_inference_model(
    site_model = create_hky_site_model(),
    beauti_options = beautier::create_beauti_options_v2_6(
      nucleotides_uppercase = TRUE,
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" # nolint indeed a long line
    )
  )
  inference_model$tree_prior$birth_rate_distr$id <- "1"
  inference_model$site_model$kappa_prior_distr$m$id <- "1"
  inference_model$site_model$kappa_prior_distr$s$id <- "2"
  inference_model$site_model$gamma_site_model$freq_prior_uniform_distr_id <- "3"

  beautier_file <- get_beautier_tempfilename(pattern = "anthus_aco_sub_2_6")
  create_beast2_input_file_from_model(
    input_filename = beautier::get_beautier_path("anthus_aco_sub.fas"),
    output_filename = beautier_file,
    inference_model = inference_model
  )
  # If this passes, this is done!
  beauti_filename <- beautier::get_beautier_path("hky_2_6.xml")
  expect_true(
    beautier::are_equivalent_xml_files(
      beautier_file,
      beauti_filename
    )
  )

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("hky_kappa_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_hky_site_model(
      id = get_alignment_id(get_fasta_filename()),
      kappa_param = create_kappa_param(value = "3.4"),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "hky_kappa_2_4.xml"
    )
  )
  # Creates temporary files in beautier folder
  compare_lines(
    lines = created,
    expected = expected
  )
  expect_true(are_equivalent_xml_lines(created, expected))
  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("hky_prop_invariant_0_5_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_hky_site_model(
      id = get_alignment_id(get_fasta_filename()),
      gamma_site_model = create_gamma_site_model(
        prop_invariant = 0.5
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("hky_prop_invariant_0_5_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("hky_gcc_1_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_hky_site_model(
      id = get_alignment_id(get_fasta_filename()),
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 1
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("hky_gcc_1_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("hky_gcc_2_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_hky_site_model(
      id = get_alignment_id(get_fasta_filename()),
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("hky_gcc_2_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("hky_gcc_4_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_hky_site_model(
      id = get_alignment_id(get_fasta_filename()),
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 4
      ),
      kappa_prior_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 1, value = "1.0"),
        s = create_s_param(id = 2, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "hky_gcc_4_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Site model: JC69
################################################################################

test_that("jc69_2_4.xml", {

  input_fasta_filename <- get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(input_fasta_filename)
  created <- create_beast2_input(
    input_filename = input_fasta_filename,
    site_model = create_jc69_site_model(
      id = id
    ),
    tree_prior = create_yule_tree_prior(
      id = id,
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )

  expected <- readLines(get_beautier_path("jc69_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("jc69_gcc_2_2_4.xml", {

  input_fasta_filename <- get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(input_fasta_filename)

  created <- create_beast2_input(
    input_filename = input_fasta_filename,
    site_model = create_jc69_site_model(
      id = id,
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "jc69_gcc_2_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("jc69_gcc_2_shape_1_5_2_4.xml", {

  input_fasta_filename <- get_beautier_path("test_output_0.fas")
  id <- get_alignment_id(input_fasta_filename)

  created <- create_beast2_input(
    input_filename = input_fasta_filename,
    site_model = create_jc69_site_model(
      id = id,
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "jc69_gcc_2_shape_1_5_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_jc69_site_model(
      id = "test_output_0",
      gamma_site_model = create_gamma_site_model(
        gamma_cat_count = 2,
        gamma_shape = 1.5,
        prop_invariant = 0.5
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )

  )
  expected <- readLines(
    get_beautier_path(
      "jc69_gcc_2_shape_1_5_prop_invariant_0_5_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Site model: TN93
################################################################################

test_that("tn93_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_tn93_site_model(
      id = get_alignment_id(get_fasta_filename()),

      kappa_1_prior_distr = create_log_normal_distr(
        id = 1,
        m = create_m_param(id = 3, value = "1.0"),
        s = create_s_param(id = 4, value = "1.25", lower = NA, upper = NA),
      ),
      kappa_2_prior_distr = create_log_normal_distr(
        id = 2,
        m = create_m_param(id = 5, value = "1.0"),
        s = create_s_param(id = 6, value = "1.25", lower = NA, upper = NA)
      )
    ),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )

  expected <- readLines(
    get_beautier_path(
      "tn93_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("tn93_gcc_1_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_tn93_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )

  expected <- readLines(
    get_beautier_path(
      "tn93_gcc_1_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("tn93_gcc_2_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_tn93_site_model(
      id = get_alignment_id(get_fasta_filename()),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )

  expected <- readLines(
    get_beautier_path(
      "tn93_gcc_2_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("tn93_kappas_not_estimated.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    site_model = create_tn93_site_model(
      id = get_alignment_id(get_fasta_filename()),
      kappa_1_param = create_kappa_1_param(value = "2.0", estimate = FALSE),
      kappa_2_param = create_kappa_2_param(value = "2.0", estimate = FALSE),
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
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options(
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "tn93_kappas_not_estimated.xml"
    )
  )
  expect_true(
    are_equivalent_xml_lines(
      remove_empty_lines(created, trim = TRUE),
      remove_empty_lines(expected, trim = TRUE),
      verbose = TRUE
    )
  )
})

################################################################################
# Tree priors
################################################################################

################################################################################
# Tree prior: BD
################################################################################

test_that("bd_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_bd_tree_prior(
      birth_rate_distr = create_uniform_distr(
        id = 3, upper = "1000.0"
      ),
      death_rate_distr = create_uniform_distr(
        id = 4, upper = NA
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "bd_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("bd_6_taxa_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_6.fas"),
    tree_prior = create_bd_tree_prior(
      birth_rate_distr = create_uniform_distr(
        id = 3, upper = "1000.0"
      ),
      death_rate_distr = create_uniform_distr(
        id = 4, upper = NA
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "bd_6_taxa_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("cbs_6_taxa_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_6.fas"),
    tree_prior = create_cbs_tree_prior(pop_sizes_scaler_scale_factor = 0.75)
  )
  expected <- readLines(
    get_beautier_path(
      "cbs_6_taxa_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("ccp_6_taxa_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_6.fas"),
    tree_prior = create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(
        id = 1,
        value = 0.3
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "ccp_6_taxa_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("cep_6_taxa_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_6.fas"),
    tree_prior = create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 2),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "cep_6_taxa_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: CBS
################################################################################

test_that("cbs_2_4.xml", {

  # Tested in detail by 'check_file_and_model_agree'
  expect_error(
    create_beast2_input(
      input_filename = get_beautier_path("test_output_0.fas"),
      tree_prior = create_cbs_tree_prior()
    ),
    "'group_sizes_dimension' \\(5\\) must be less than the number of taxa \\(5\\)" # nolint
  )
})

test_that("cbs_6_taxa_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_6.fas"),
    tree_prior = create_cbs_tree_prior(
      pop_sizes_scaler_scale_factor = 0.75
    )
  )
  expected <- readLines(
    get_beautier_path(
      "cbs_6_taxa_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_cbs_groupsize_4_dim.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_cbs_tree_prior(
      group_sizes_dimension = 4,
      pop_sizes_scaler_scale_factor = 0.75
    ),
    beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
  )
  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_cbs_groupsize_4_dim.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: CCP
################################################################################

test_that("ccp_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_ccp_tree_prior(
      pop_size_distr = create_one_div_x_distr(
        id = 1,
        value = 0.3
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "ccp_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("ccp_pop_size_gamma_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_ccp_tree_prior(
      pop_size_distr = create_gamma_distr(
        id = 2,
        alpha = create_alpha_param(id = 9, value = "2.0"),
        beta = create_beta_param(id = 10, value = "2.0"),
        value = 0.3
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "ccp_pop_size_gamma_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: CEP
################################################################################

test_that("cep_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 1, value = "0.001"),
        scale = create_scale_param(id = 2, value = "30.701135")
      )
    )
  )
  expected <- readLines(get_beautier_path("cep_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tree prior: Yule
################################################################################

test_that("yule_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(get_beautier_path("yule_2_4.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Prior distributions
################################################################################
test_that("birth_rate_uniform_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_uniform_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_normal_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_normal_distr(
        id = 0,
        mean = create_mean_param(id = 1, value = "0.0"),
        sigma = create_sigma_param(id = 2, value = "1.0")
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_normal_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_one_div_x_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_one_div_x_distr(id = 1)
    )
  )

  expected <- readLines(
    get_beautier_path(
      "birth_rate_one_div_x_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_log_normal_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_log_normal_distr(
        id = 0,
        m = create_m_param(id = 3, value = "1.0"),
        s = create_s_param(
          id = 4,
          value = "1.25",
          lower = "0.0",
          upper = "5.0"
        )
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_log_normal_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_exp_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_exp_distr(
        id = 1,
        mean = create_mean_param(id = 5, value = "1.0")
      )
    )
  )

  expected <- readLines(
    get_beautier_path(
      "birth_rate_exp_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})


test_that("birth_rate_gamma_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 6, value = "2.0"),
        beta = create_beta_param(id = 7, value = "2.0")
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_gamma_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_beta_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_beta_distr(
        id = 0,
        alpha = create_alpha_param(id = 8, value = "2.0"),
        beta = create_beta_param(id = 9, value = "2.0")
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_beta_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_laplace_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 10, value = "0.0"),
        scale = create_scale_param(id = 11, value = "1.0")
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_laplace_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_inv_gamma_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_inv_gamma_distr(
        id = 0,
        alpha = create_alpha_param(
          id = 12,
          value = "2.0"
        ),
        beta = create_beta_param(
          id = 13,
          value = "2.0"
        )
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_inv_gamma_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("birth_rate_poisson_2_4.xml", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("test_output_0.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_poisson_distr(
        id = 0,
        lambda = create_lambda_param(id = 14, value = "0.693")
      )
    )
  )
  expected <- readLines(
    get_beautier_path(
      "birth_rate_poisson_2_4.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# MCMC options
################################################################################

################################################################################
# MRCA priors
################################################################################
# No calibration yet
test_that("anthus_aco_sub.xml", {
  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibration.xml", {

  # This XML file has a normal distribution for the MRCA prior
  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- create_beast2_input(
    input_filename = fasta_filename,
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_prior = create_mrca_prior(
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
    beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_calibrated.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibrated_no_prior.xml", {
  # This XML file has no prior distribution

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- create_beast2_input(
    input_filename = fasta_filename,
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_prior = create_mrca_prior(
      name = "every",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      clock_prior_distr_id = 0
    ),
    beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_calibrated_no_prior.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibrated_rln.xml", {

  # This XML file has
  # - an MRCA prior distribution prior,
  # - an RLN clock model

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- create_beast2_input(
    input_filename = fasta_filename,
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 6,
        alpha = create_alpha_param(id = 21, value = "0.5396"),
        beta = create_beta_param(id = 22, value = "0.3819")
      ),
      mparam_id = 20
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_prior = create_mrca_prior(
      name = "every",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      clock_prior_distr_id = 0
    ),
    beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
  )
  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_calibrated_rln.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("anthus_aco_sub_calibrated_rln_monophyletic.xml", {

  # This XML file has
  # - an MRCA prior distribution prior,
  # - an RLN clock model, that is monophyletic

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- create_beast2_input(
    input_filename = fasta_filename,
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 6,
        alpha = create_alpha_param(id = 21, value = "0.5396"),
        beta = create_beta_param(id = 22, value = "0.3819")
      ),
      mparam_id = 20
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_prior = create_mrca_prior(
      name = "every",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      clock_prior_distr_id = 0,
      is_monophyletic = TRUE
    ),
    beauti_options = create_beauti_options(nucleotides_uppercase = TRUE)
  )
  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_calibrated_rln_monophyletic.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# MRCA priors again 2018-10-26
################################################################################
test_that("Base point: anthus_aco_sub.xml", {
  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options(
      nucleotides_uppercase = TRUE,
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_20181016.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})


test_that("Base point + all taxa", {
  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mrca_prior = create_mrca_prior(
      name = "all",
      alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
      taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
      is_monophyletic = FALSE
    ),
    beauti_options = create_beauti_options(
      nucleotides_uppercase = TRUE,
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_20181016_all.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})


test_that("Base point + all taxa + monophyletic", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mrca_prior = create_mrca_prior(
      name = "all",
      alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
      taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
      is_monophyletic = TRUE
    ),
    beauti_options = create_beauti_options(
      nucleotides_uppercase = TRUE,
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_20181016_all_monophyletic.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Base point + all taxa + monophyletic + one_div_x", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mrca_prior = create_mrca_prior(
      name = "all",
      alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
      taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
      is_monophyletic = TRUE,
      mrca_distr = create_one_div_x_distr(id = 1),
      clock_prior_distr_id = 0
    ),
    beauti_options = create_beauti_options(
      nucleotides_uppercase = TRUE,
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_20181016_all_one_div_x_monophyletic.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Base point + all taxa + one_div_x", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mrca_prior = create_mrca_prior(
      name = "all",
      alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
      taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
      is_monophyletic = FALSE,
      mrca_distr = create_one_div_x_distr(id = 1),
      clock_prior_distr_id = 0
    ),
    beauti_options = create_beauti_options(
      nucleotides_uppercase = TRUE,
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_20181016_all_one_div_x.xml"
    )
  )

  expect_true(are_equivalent_xml_lines(created, expected))
})


test_that("Base point + all taxa + one_div_x + RLN", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("anthus_aco_sub.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    clock_model = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(id = 6,
        alpha = create_alpha_param(id = 21, value = "0.5396"),
        beta = create_beta_param(id = 22, value = "0.3819")
      ),
      mparam_id = 20,
      mean_rate_prior_distr = create_uniform_distr(id = 4)
    ),
    mrca_prior = create_mrca_prior(
      name = "all",
      alignment_id = get_alignment_id(get_beautier_path("anthus_aco_sub.fas")),
      taxa_names = get_taxa_names(get_beautier_path("anthus_aco_sub.fas")),
      is_monophyletic = FALSE,
      mrca_distr = create_one_div_x_distr(id = 1),
      clock_prior_distr_id = 0
    ),
    beauti_options = create_beauti_options(
      nucleotides_uppercase = TRUE,
      beast2_version = "2.5",
      required = "BEAST v2.5.0"
    )
  )

  expected <- readLines(
    get_beautier_path(
      "anthus_aco_sub_20181016_all_one_div_x_rln.xml"
    )
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

################################################################################
# Tip dating
################################################################################
test_that("No tip dating yet", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("G_VII_pre2003_msa.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    beauti_options = create_beauti_options(
      beast2_version = "2.5",
      required = "BEAST v2.5.0",
      nucleotides_uppercase = TRUE
    )
  )

  expected <- readLines(get_beautier_path("G_VII_pre2003_no_tip.xml"))
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Tip dating, v2.5", {

  created <- create_beast2_input(
    input_filename = get_beautier_path("G_VII_pre2003_msa.fas"),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    tipdates_filename = get_beautier_path("G_VII_pre2003_dates_4.txt"),
    beauti_options = create_beauti_options(
      beast2_version = "2.5",
      required = "BEAST v2.5.1",
      nucleotides_uppercase = TRUE,
      sequence_indent = 24
    )
  )
  read_tipdates_file(get_beautier_path("G_VII_pre2003_dates_4.txt"))
  expected <- readLines(get_beautier_path("G_VII_pre2003.xml"))
  compare_lines(
    lines = created,
    expected = expected,
    created_lines_filename = "~/created.xml",
    expected_lines_filename = "~/expected.xml"
  )

  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("Tip dating, v2.6", {
  inference_model <- create_inference_model(
    clock_model = create_strict_clock_model(rate_scaler_factor = ""),
    tree_prior = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    tipdates_filename = get_beautier_path("test_output_0_tipdates.tsv"),
    beauti_options = create_beauti_options_v2_6(
      namespace = "beast.core:beast.evolution.alignment:beast.evolution.tree.coalescent:beast.core.util:beast.evolution.nuc:beast.evolution.operators:beast.evolution.sitemodel:beast.evolution.substitutionmodel:beast.evolution.likelihood" # nolint indeed a long line
    )
  )
  inference_model <- init_inference_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )

  created <- create_beast2_input_from_model(
    input_filename = get_beautier_path("test_output_0.fas"),
    inference_model = inference_model
  )
  expected <- readLines(get_beautier_path("tipdates_2_6.xml"))

  # Creates temporary files in beautier folder
  if (1 == 2) {
    compare_lines(
      lines = created,
      expected = expected,
      created_lines_filename = "~/created.xml",
      expected_lines_filename = "~/expected.xml",
    )
  }
  expect_true(are_equivalent_xml_lines(created, expected))
  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("Tip dating with RLN 1/3", {

  skip("Issue #116. RLN + tipdating")
  # Probably needs to get proper IDs and such ...
  # Prerequisite for https://github.com/ropensci/babette/issues/90
  created <- create_beast2_input(
    input_filename = get_beautier_path("Felinecoronavirus_Envelope_1.fas"),
    tipdates_filename = get_beautier_path(
      "Felinecoronavirus_Envelope_1_no_quotes.txt"
    ),
    beauti_options = create_beauti_options(
      beast2_version = "2.6",
      nucleotides_uppercase = TRUE,
      sequence_indent = 8
    )
  )
  expected <- readLines(get_beautier_path("Felinecoronavirus_Envelope_1.xml"))

  # Creates temporary files in beautier folder
  compare_lines(
    lines = created,
    expected = expected
  )
  expect_true(are_equivalent_xml_lines(created, expected, verbose = TRUE))

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("RLN and non-monophyletic MRCA with distribution, beastier", {
  # babette issue 106. babette Issue #106
  # https://github.com/ropensci/babette/issues/106

  # Thanks to Raphael Scherrer for sharing this bug
  fasta_filename <- get_fasta_filename()
  lines <- beautier::create_beast2_input(
    input_filename = fasta_filename,
    clock_model = create_rln_clock_model(),
    mrca_prior = create_mrca_prior(
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      is_monophyletic = FALSE,
      mrca_distr = create_one_div_x_distr() # Use simpler distribution
    ),
    beauti_options = create_beauti_options_v2_6()
  )
  # The next testing function does compare line-by-line
  if ("beastier" %in% installed.packages()[, 1]) {
    if (beastier::is_beast2_installed()) {
      expect_true(
        beastier::are_beast2_input_lines(
          lines, method = "deep"
        )
      )
    }
  }

  remove_beautier_folder()
  check_empty_beautier_folder()
})

test_that("RLN and non-monophyletic MRCA with distribution, beastier", {
  # This is the same test as above, yet made to function with beautier
  # functionality only. It does mess up the cleanliness of
  # the creation of the inference model
  # babette issue 106. babette Issue #106
  # https://github.com/ropensci/babette/issues/106

  # Thanks to Raphael Scherrer for sharing this bug
  fasta_filename <- get_fasta_filename()
  lines <- beautier::create_beast2_input(
    input_filename = fasta_filename,
    tree_prior = create_tree_prior_yule(birth_rate_distr = create_uniform_distr(id = "1")),
    clock_model = create_rln_clock_model(
      mparam_id = "1",
      mean_rate_prior_distr = create_uniform_distr(id = "3"),
      ucldstdev_distr = create_gamma_distr(
        id = "0",
        alpha = create_alpha_param(id = "2", value = "0.5396"),
        beta = create_beta_param(id = "3", value = "0.3819")
      )
    ),
    mrca_prior = create_mrca_prior(
      name = "all_taxa",
      alignment_id = get_alignment_id(fasta_filename),
      taxa_names = get_taxa_names(fasta_filename),
      is_monophyletic = FALSE,
      mrca_distr = create_one_div_x_distr(id = "1") # Use simpler distribution
    ),
    beauti_options = create_beauti_options_v2_6()
  )
  if ("beastier" %in% installed.packages()[,1]) {
    if (beastier::is_beast2_installed()) {
      expect_true(
        beastier::are_beast2_input_lines(
          lines, method = "deep"
        )
      )
    }
  }
  expected <- readLines(get_beautier_path("babette_issue_26.xml"))

  # Creates temporary files in beautier folder
  created <- lines
  compare_lines(
    lines = lines,
    expected = expected,
    created_lines_filename = "~/created.txt",
    expected_lines_filename = "~/expected.txt"
  )
  expect_true(are_equivalent_xml_lines(created, expected, verbose = TRUE))
  remove_beautier_folder()
  check_empty_beautier_folder()
})
