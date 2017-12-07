context("site_models_to_xml_state")

test_that("JC69 JC69 JC69 JC69", {

  expected <- c(
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_nd2"),
      create_jc69_site_model(id = "anthus_nd3"),
      create_jc69_site_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("HKY HKY HKY HKY", {

  expected <- c(
    "<parameter id=\"kappa.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_aco\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>" # nolint XML can be long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_hky_site_model(id = "anthus_aco"),
      create_hky_site_model(id = "anthus_nd2"),
      create_hky_site_model(id = "anthus_nd3"),
      create_hky_site_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("TN93 TN93 TN93 TN93", {

  expected <- c(
    "<parameter id=\"kappa1.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa2.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa1.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa2.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa1.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa2.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa1.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"kappa2.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_aco\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>" # nolint XML can be long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_tn93_site_model(id = "anthus_aco"),
      create_tn93_site_model(id = "anthus_nd2"),
      create_tn93_site_model(id = "anthus_nd3"),
      create_tn93_site_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("GTR GTR GTR GTR", {

  expected <- c(
    "<parameter id=\"rateAC.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAG.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAT.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateCG.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateGT.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAC.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAG.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAT.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateCG.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateGT.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAC.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAG.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAT.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateCG.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateGT.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAC.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAG.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateAT.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateCG.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"rateGT.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_aco\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML can be long
    "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>"  # nolint XML can be long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_gtr_site_model(id = "anthus_aco"),
      create_gtr_site_model(id = "anthus_nd2"),
      create_gtr_site_model(id = "anthus_nd3"),
      create_gtr_site_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})



test_that("HKY 0 HKY 1 HKY 2 HKY 3 GCC", {

  expected <- c(
    "<parameter id=\"kappa.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd4\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_aco\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_hky_site_model(id = "anthus_aco",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 0)),
      create_hky_site_model(id = "anthus_nd2",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)),
      create_hky_site_model(id = "anthus_nd3",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)),
      create_hky_site_model(id = "anthus_nd4",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 3))
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("JC69 1 JC69 1 JC69 1 JC69 1 GCC", {

  expected <- c(
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)
      ),
      create_jc69_site_model(id = "anthus_nd2",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)
      ),
      create_jc69_site_model(id = "anthus_nd3",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)
      ),
      create_jc69_site_model(id = "anthus_nd4",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})


test_that("JC69 2 JC69 2 JC69 2 JC69 2 GCC", {

  expected <- c(
    "<parameter id=\"gammaShape.s:anthus_aco\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd2\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd3\" name=\"stateNode\">1.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd4\" name=\"stateNode\">1.0</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)
      ),
      create_jc69_site_model(id = "anthus_nd2",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)
      ),
      create_jc69_site_model(id = "anthus_nd3",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)
      ),
      create_jc69_site_model(id = "anthus_nd4",
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("JC69 2 JC69 2 JC69 2 JC69 2 GCC diff shapes diff prop_inv", {

  expected <- c(
    "<parameter id=\"gammaShape.s:anthus_aco\" name=\"stateNode\">1.1</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd2\" name=\"stateNode\">1.2</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd3\" name=\"stateNode\">1.3</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd4\" name=\"stateNode\">1.4</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.1", prop_invariant = 0.1)
      ),
      create_jc69_site_model(id = "anthus_nd2",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.2", prop_invariant = 0.2)
      ),
      create_jc69_site_model(id = "anthus_nd3",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.3", prop_invariant = 0.3)
      ),
      create_jc69_site_model(id = "anthus_nd4",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.4", prop_invariant = 0.4)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("JC69 JC69 JC69 JC69 shared site model", {

  expected <- c(
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("HKY 2 HKY 2 HKY 2 HKY 2 GCC diff shapes diff prop_inv", {

  expected <- c(
    "<parameter id=\"gammaShape.s:anthus_aco\" name=\"stateNode\">1.1</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd2\" name=\"stateNode\">1.2</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd3\" name=\"stateNode\">1.3</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"gammaShape.s:anthus_nd4\" name=\"stateNode\">1.4</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_aco\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"kappa.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>", # nolint XML is long, so this line is long
    "<parameter id=\"freqParameter.s:anthus_aco\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>" # nolint XML is long, so this line is long
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_hky_site_model(id = "anthus_aco",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.1", prop_invariant = 0.1)
      ),
      create_hky_site_model(id = "anthus_nd2",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.2", prop_invariant = 0.2)
      ),
      create_hky_site_model(id = "anthus_nd3",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.3", prop_invariant = 0.3)
      ),
      create_hky_site_model(id = "anthus_nd4",
        gamma_site_model = create_gamma_site_model(
          gamma_cat_count = 2, gamma_shape = "1.4", prop_invariant = 0.4)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("JC69 HKY TN93 GTR", {

  expected <- c(
    "<parameter id=\"kappa.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>",
    "<parameter id=\"kappa1.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>",
    "<parameter id=\"kappa2.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.0</parameter>",
    "<parameter id=\"rateAC.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>",
    "<parameter id=\"rateAG.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>",
    "<parameter id=\"rateAT.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>",
    "<parameter id=\"rateCG.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>",
    "<parameter id=\"rateGT.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.0</parameter>",
    "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>",
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>",
    "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
  )
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_hky_site_model(id = "anthus_nd2"),
      create_tn93_site_model(id = "anthus_nd3"),
      create_gtr_site_model(id = "anthus_nd4")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))

})

test_that("aco_nd2_nd3_nd4_complex_2_4.xml state section", {

  expected <- c(
    "<parameter id=\"kappa.s:anthus_nd2\" lower=\"0.0\" name=\"stateNode\">2.1</parameter>",
    "<parameter id=\"kappa1.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.2</parameter>",
    "<parameter id=\"kappa2.s:anthus_nd3\" lower=\"0.0\" name=\"stateNode\">2.3</parameter>",
    "<parameter id=\"rateAC.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.1</parameter>",
    "<parameter id=\"rateAG.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.2</parameter>",
    "<parameter id=\"rateAT.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.3</parameter>",
    "<parameter id=\"rateCG.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.4</parameter>",
    "<parameter id=\"rateGT.s:anthus_nd4\" lower=\"0.0\" name=\"stateNode\">1.6</parameter>",
    "<parameter id=\"gammaShape.s:anthus_aco\" name=\"stateNode\">0.1</parameter>",
    "<parameter id=\"gammaShape.s:anthus_nd2\" name=\"stateNode\">0.2</parameter>",
    "<parameter id=\"gammaShape.s:anthus_nd3\" name=\"stateNode\">0.4</parameter>",
    "<parameter id=\"freqParameter.s:anthus_nd2\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
  )

  # JC69, HKY, TN93, GTR
  created <- beautier:::site_models_to_xml_state(
    site_models = list(
      create_jc69_site_model(
        id = "anthus_aco",
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
        id = "anthus_nd2",
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
        id = "anthus_nd3",
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
        id = "anthus_nd4",
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
    )
  )
  should_be_absent_1 <- "<parameter id=\"freqParameter.s:anthus_nd3\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
  should_be_absent_2 <- "<parameter id=\"freqParameter.s:anthus_nd4\" dimension=\"4\" lower=\"0.0\" name=\"stateNode\" upper=\"1.0\">0.25</parameter>"
  testthat::expect_equal(0, sum(created == should_be_absent_1))
  testthat::expect_equal(0, sum(created == should_be_absent_2))
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected, verbose = TRUE))
})
