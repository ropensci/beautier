context("site_models_to_xml_state")

test_that("JC69 JC69 JC69 JC69", {

  skip("WIP")

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

  skip("WIP")

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

  skip("WIP")

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

  skip("WIP")

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
