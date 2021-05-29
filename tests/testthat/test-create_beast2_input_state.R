test_that("v2.4", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model()
  )
  created <- create_beast2_input_state(
    inference_model = inference_model
  )
  expected <- c(
    "<state id=\"state\" storeEvery=\"5000\">",
    "    <tree id=\"Tree.t:test_output_0\" name=\"stateNode\">",
    "        <taxonset id=\"TaxonSet.test_output_0\" spec=\"TaxonSet\">",
    "            <alignment idref=\"test_output_0\"/>",
    "        </taxonset>",
    "    </tree>",
    "    <parameter id=\"birthRate.t:test_output_0\" name=\"stateNode\">1.0</parameter>", # nolint long line indeed
    "</state>"
  )
  expect_equal(created, expected)
})

test_that("v2.6", {

  inference_model <- init_inference_model(
    input_filename = get_fasta_filename(),
    inference_model = create_test_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  expect_silent(create_beast2_input_state(inference_model = inference_model))
})
