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
