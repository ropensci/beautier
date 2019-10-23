test_that("use", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model()
  )
  created <- create_beast2_input_tracelog(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expected <- c(
    "<logger id=\"tracelog\" fileName=\"test_output_0.log\" logEvery=\"1000\" model=\"@posterior\" sanitiseHeaders=\"true\" sort=\"smart\">", # nolint
    "    <log idref=\"posterior\"/>", # nolint this is no absolute path
    "    <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "    <log idref=\"prior\"/>", # nolint this is no absolute path
    "    <log idref=\"treeLikelihood.test_output_0\"/>", # nolint this is no absolute path
    "    <log id=\"TreeHeight.t:test_output_0\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:test_output_0\"/>",  # nolint this is no absolute path
    "    <log idref=\"YuleModel.t:test_output_0\"/>", # nolint this is no absolute path
    "    <log idref=\"birthRate.t:test_output_0\"/>", # nolint this is no absolute path
    "</logger>"
  )
  expect_equal(created, expected)
})
