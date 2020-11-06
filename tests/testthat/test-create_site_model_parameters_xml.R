test_that("use, JC69, v2.4", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(),
      beauti_options = create_beauti_options_v2_4()
    )
  )
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>" # nolint long line indeed
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
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>" # nolint long line indeed
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
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>" # nolint long line indeed
  )
  expect_equal(created, expected)
})

test_that("use, JC69, v2.6", {
  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      site_model = create_jc69_site_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" spec=\"parameter.RealParameter\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "                    ",
    "<parameter id=\"gammaShape.s:test_output_0\" spec=\"parameter.RealParameter\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "                    ",
    "<parameter id=\"proportionInvariant.s:test_output_0\" spec=\"parameter.RealParameter\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>", # nolint long line indeed
    "                    "
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
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>" # nolint long line indeed
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
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>" # nolint long line indeed
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
  created <- create_site_model_parameters_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<parameter id=\"mutationRate.s:test_output_0\" estimate=\"false\" name=\"mutationRate\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"gammaShape.s:test_output_0\" estimate=\"false\" name=\"shape\">1.0</parameter>", # nolint long line indeed
    "<parameter id=\"proportionInvariant.s:test_output_0\" estimate=\"false\" lower=\"0.0\" name=\"proportionInvariant\" upper=\"1.0\">0.0</parameter>" # nolint long line indeed
  )
  expect_equal(created, expected)
})
