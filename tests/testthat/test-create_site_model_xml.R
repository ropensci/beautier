test_that("use, JC69, v2.4", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "    <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "    <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>",
    "</siteModel>"
  )
  expect_equal(created, expected)
})

test_that("use, JC69, v2.4, GCC = 1", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 1)
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\" gammaCategoryCount=\"1\">",
    "    <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "    <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>",
    "</siteModel>"
  )
  expect_equal(created, expected)
})

test_that("use, JC69, v2.4, GCC = 2", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(
        gamma_site_model = create_gamma_site_model(gamma_cat_count = 2)
      ),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\" gammaCategoryCount=\"2\" shape=\"@gammaShape.s:test_output_0\">", # nolint long line indeed
    "    <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "    <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>",
    "</siteModel>"
  )
  expect_equal(created, expected)
})

test_that("use, JC69, v2.6", {
  skip("WIP")
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "                        ",
    "    <parameter id=\"mutationRate.s:test_output_0\" spec=\"parameter.RealParameter\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>",
    "                        ",
    "    <parameter id=\"gammaShape.s:test_output_0\" spec=\"parameter.RealParameter\" estimate=\"false\" name=\"shape\">1.0</parameter>",
    "                        ",
    "    <parameter id=\"proportionInvariant.s:test_output_0\" spec=\"parameter.RealParameter\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>",
    "                        ",
    "    <substModel id=\"JC69.s:test_output_0\" spec=\"JukesCantor\"/>",
    "                    ",
    "</siteModel>"
  )
  expect_equal(created, expected)
})

test_that("use, HKY", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_hky_site_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "    <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "    <substModel id=\"hky.s:test_output_0\" spec=\"HKY\" kappa=\"@kappa.s:test_output_0\">", # nolint long line indeed
    "        <frequencies id=\"estimatedFreqs.s:test_output_0\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:test_output_0\"/>", # nolint long line indeed
    "    </substModel>",
    "</siteModel>"
  )
  expect_equal(created, expected)
})

test_that("use, TN93", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_tn93_site_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "    <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "    <substModel id=\"tn93.s:test_output_0\" spec=\"TN93\" kappa1=\"@kappa1.s:test_output_0\" kappa2=\"@kappa2.s:test_output_0\">", # nolint long line indeed
    "        <frequencies id=\"estimatedFreqs.s:test_output_0\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:test_output_0\"/>", # nolint long line indeed
    "    </substModel>",
    "</siteModel>"
  )
  expect_equal(created, expected)
})

test_that("use, GTR", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_gtr_site_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<siteModel id=\"SiteModel.s:test_output_0\" spec=\"SiteModel\">",
    "    <parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "    <parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "    <substModel id=\"gtr.s:test_output_0\" spec=\"GTR\" rateAC=\"@rateAC.s:test_output_0\" rateAG=\"@rateAG.s:test_output_0\" rateAT=\"@rateAT.s:test_output_0\" rateCG=\"@rateCG.s:test_output_0\" rateGT=\"@rateGT.s:test_output_0\">", # nolint long line indeed
    "        <parameter id=\"rateCT.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"rateCT\">1.0</parameter>", # nolint long line indeed
    "        <frequencies id=\"estimatedFreqs.s:test_output_0\" spec=\"Frequencies\" frequencies=\"@freqParameter.s:test_output_0\"/>", # nolint long line indeed
    "    </substModel>",
    "</siteModel>"
  )
  expect_equal(created, expected)
})
