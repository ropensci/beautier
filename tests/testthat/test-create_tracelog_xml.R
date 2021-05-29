test_that("minimal use", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model()
  )
  expect_silent(
    create_tracelog_xml(
      input_filename = input_filename,
      inference_model = inference_model
    )
  )
  expect_true(
    is_xml(
      create_tracelog_xml(
        input_filename = input_filename,
        inference_model = inference_model
      )
    )
  )
})

test_that("detailed use, ?v2.4", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model()
  )
  created <- create_tracelog_xml(
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

test_that("detailed use, v2.6", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expected <- c(
    "<logger id=\"tracelog\" spec=\"Logger\" fileName=\"test_output_0.log\" logEvery=\"1000\" model=\"@posterior\" sanitiseHeaders=\"true\" sort=\"smart\">", # nolint
    "    <log idref=\"posterior\"/>", # nolint
    "    <log idref=\"likelihood\"/>", # nolint
    "    <log idref=\"prior\"/>", # nolint
    "    <log idref=\"treeLikelihood.test_output_0\"/>", # nolint
    "    <log id=\"TreeHeight.t:test_output_0\" spec=\"beast.evolution.tree.TreeHeightLogger\" tree=\"@Tree.t:test_output_0\"/>", # nolint
    "    <log idref=\"YuleModel.t:test_output_0\"/>", # nolint
    "    <log idref=\"birthRate.t:test_output_0\"/>", # nolint
    "</logger>" # nolint
  )
  expect_equal(created, expected)
})
test_that("file_name in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        tracelog = create_tracelog(
          filename = "my_file.txt"
        )
      )
    )
  )

  xml <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "fileName=\"my_file.txt\"")[1, 1]
    )
  )
})

test_that("log_every in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        tracelog = create_tracelog(
          log_every = 1234
        )
      )
    )
  )

  xml <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "logEvery=\"1234\"")[1, 1]
    )
  )
})

test_that("mode in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        tracelog = create_tracelog(
          mode = "compound"
        )
      )
    )
  )

  xml <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "mode=\"compound\"")[1, 1]
    )
  )
})

test_that("sanitise_headers in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        tracelog = create_tracelog(
          sanitise_headers = TRUE
        )
      )
    )
  )

  xml <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "sanitiseHeaders=\"true\"")[1, 1]
    )
  )
})

test_that("sort in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        tracelog = create_tracelog(
          sort = "smart"
        )
      )
    )
  )

  xml <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "sort=\"smart\"")[1, 1]
    )
  )
})
