test_that("use, v2.4", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model()
  )
  created <- create_treelog_xml(
    inference_model = inference_model
  )
  # create_treelog_xml starts with an empty line. Issue #109
  expected <- c(
    "",
    "<logger id=\"treelog.t:test_output_0\" fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">", # nolint
    "    <log id=\"TreeWithMetaDataLogger.t:test_output_0\" spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" tree=\"@Tree.t:test_output_0\"/>", # nolint long line indeed
    "</logger>"
  )
  expect_equal(created, expected)
})

test_that("use, v2.6", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model(
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_treelog_xml(
    inference_model = inference_model
  )
  # create_treelog_xml starts with an empty line. Issue #109
  expected <- c(
    "",
    "<logger id=\"treelog.t:test_output_0\" spec=\"Logger\" fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">", # nolint
    "    <log id=\"TreeWithMetaDataLogger.t:test_output_0\" spec=\"beast.evolution.tree.TreeWithMetaDataLogger\" tree=\"@Tree.t:test_output_0\"/>", # nolint
    "</logger>" # nolint
  )
  expect_equal(created, expected)
})

test_that("use", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model()
  )


  expect_silent(
    create_treelog_xml(
      inference_model = inference_model
    )
  )
  # -1 because create_treelog_xml starts with an empty line. Issue #109
  expect_true(
    is_xml(
      create_treelog_xml(
        inference_model = inference_model
      )[-1]
    )
  )
})

test_that("file_name in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        treelog = create_treelog(
          filename = "my_file.txt"
        )
      )
    )
  )

  xml <- create_treelog_xml(
    inference_model = inference_model
  )
  # 2 because create_treelog_xml starts with an empty line. Issue #109
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "fileName=\"my_file.txt\"")[2, 1]
    )
  )
})

test_that("log_every in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        treelog = create_treelog(
          log_every = 1234
        )
      )
    )
  )

  xml <- create_treelog_xml(
    inference_model = inference_model
  )
  # 2 because create_treelog_xml starts with an empty line. Issue #109
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "logEvery=\"1234\"")[2, 1]
    )
  )
})

test_that("mode in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        treelog = create_treelog(
          mode = "compound"
        )
      )
    )
  )

  xml <- create_treelog_xml(
    inference_model = inference_model
  )
  # 2 because create_treelog_xml starts with an empty line. Issue #109
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "mode=\"compound\"")[2, 1]
    )
  )
})

test_that("sanitise_headers in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        treelog = create_treelog(
          sanitise_headers = TRUE
        )
      )
    )
  )

  xml <- create_treelog_xml(
    inference_model = inference_model
  )
  # 2 because create_treelog_xml starts with an empty line. Issue #109
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "sanitiseHeaders=\"true\"")[2, 1]
    )
  )
})

test_that("sort in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        treelog = create_treelog(
          sort = "smart"
        )
      )
    )
  )

  xml <- create_treelog_xml(
    inference_model = inference_model
  )
  # 2 because create_treelog_xml starts with an empty line. Issue #109
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "sort=\"smart\"")[2, 1]
    )
  )
})
