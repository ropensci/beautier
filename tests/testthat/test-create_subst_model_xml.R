test_that("use, JC69", {

  inference_model <- create_inference_model(
    site_model = create_jc69_site_model(id = 123)
  )
  created <- create_subst_model_xml(
    inference_model = inference_model
  )
  expected <- "<substModel id=\"JC69.s:123\" spec=\"JukesCantor\"/>"
  expect_equal(created, expected)
})

test_that("use, HKY", {

  inference_model <- create_inference_model(
    create_hky_site_model(id = 123)
  )

  created <- create_subst_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<substModel id=\"hky.s:123\" spec=\"HKY\" kappa=\"@kappa.s:123\">",
    "    <frequencies id=\"estimatedFreqs.s:123\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:123\"/>", # nolint indeed a long line
    "</substModel>"
  )
  expect_equal(created, expected)
})

test_that("use, TN93", {

  inference_model <- create_inference_model(
    site_model = create_tn93_site_model(id = 123)
  )
  created <- create_subst_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<substModel id=\"tn93.s:123\" spec=\"TN93\" kappa1=\"@kappa1.s:123\" kappa2=\"@kappa2.s:123\">", # nolint indeed a long line
    "    <frequencies id=\"estimatedFreqs.s:123\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:123\"/>", # nolint indeed a long line
    "</substModel>"
  )
  expect_equal(created, expected)
})

test_that("use, GTR", {

  inference_model <- create_inference_model(
    site_model = create_gtr_site_model(id = 123)
  )

  created <- create_subst_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<substModel id=\"gtr.s:123\" spec=\"GTR\" rateAC=\"@rateAC.s:123\" rateAG=\"@rateAG.s:123\" rateAT=\"@rateAT.s:123\" rateCG=\"@rateCG.s:123\" rateGT=\"@rateGT.s:123\">", # nolint indeed a long line
    "    <parameter id=\"rateCT.s:123\" estimate=\"false\" lower=\"0.0\" name=\"rateCT\">1.0</parameter>", # nolint indeed a long line
    "    <frequencies id=\"estimatedFreqs.s:123\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:123\"/>", # nolint indeed a long line
    "</substModel>"
  )
  expect_equal(created, expected)
})
