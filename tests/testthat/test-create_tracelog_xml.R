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

test_that("detailed use, v2.4", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model()
  )
  created <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expected <- unindent(
      extract_xml_section_from_lines(
      readr::read_lines(get_beautier_path("2_4.xml")),
      section = "logger"
    )[1:9]
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
  expected <- unindent(
    remove_empty_lines(
      extract_xml_section_from_lines(
        readr::read_lines(get_beautier_path("2_6_2.xml")),
        section = "logger"
      )
    )[1:9]
  )
  expect_equal(created, expected)
})

test_that("detailed use, v2.6, RLN", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model(
      clock_model = create_rln_clock_model(),
      beauti_options = create_beauti_options_v2_6()
    )
  )
  created <- create_tracelog_xml(
    input_filename = input_filename,
    inference_model = inference_model
  )
  expected <- unindent(
    remove_empty_lines(
      extract_xml_section_from_lines(
        readr::read_lines(get_beautier_path("rln_2_6.xml")),
        section = "logger"
      )
    )[1:11]
  )
  expect_true(are_equivalent_xml_lines(created, expected))
  expect_equal(sort(created), sort(expected))
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
