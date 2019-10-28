test_that("use", {

  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    create_inference_model()
  )
  created <- create_screenlog_xml(
    inference_model = inference_model
  )
  expected <- c(
    "<logger id=\"screenlog\" logEvery=\"1000\">", # nolint this is no absolute path
    "    <log idref=\"posterior\"/>", # nolint this is no absolute path
    "    <log id=\"ESS.0\" spec=\"util.ESS\" arg=\"@posterior\"/>", # nolint this is no absolute path
    "    <log idref=\"likelihood\"/>", # nolint this is no absolute path
    "    <log idref=\"prior\"/>", # nolint this is no absolute path
    "</logger>"
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
    create_screenlog_xml(
      inference_model = inference_model
    )
  )
  expect_true(
    is_xml(
      create_screenlog_xml(
        inference_model = inference_model
      )
    )
  )
})

test_that("file_name in XML", {
  input_filename <- get_fasta_filename()
  inference_model <- init_inference_model(
    input_filename = input_filename,
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        screenlog = create_screenlog(
          filename = "my_file.txt"
        )
      )
    )
  )

  xml <- create_screenlog_xml(
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
        screenlog = create_screenlog(
          log_every = 1234
        )
      )
    )
  )

  xml <- create_screenlog_xml(
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
        screenlog = create_screenlog(
          mode = "compound"
        )
      )
    )
  )

  xml <- create_screenlog_xml(
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
        screenlog = create_screenlog(
          sanitise_headers = TRUE
        )
      )
    )
  )

  xml <- create_screenlog_xml(
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
        screenlog = create_screenlog(
          sort = "smart"
        )
      )
    )
  )

  xml <- create_screenlog_xml(
    inference_model = inference_model
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "sort=\"smart\"")[1, 1]
    )
  )
})
