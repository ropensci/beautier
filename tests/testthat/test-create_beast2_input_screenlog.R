test_that("use", {
  expect_silent(create_beast2_input_screenlog())
  expect_true(is_xml(create_beast2_input_screenlog()))
})

test_that("file_name in XML", {
  xml <- create_beast2_input_screenlog(
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        screenlog = create_screenlog(
          filename = "my_file.txt"
        )
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "fileName=\"my_file.txt\"")[1, 1]
    )
  )
})

test_that("log_every in XML", {
  xml <- create_beast2_input_screenlog(
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        screenlog = create_screenlog(
          log_every = 1234
        )
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "logEvery=\"1234\"")[1, 1]
    )
  )
})

test_that("mode in XML", {
  xml <- create_beast2_input_screenlog(
    inference_model = create_inference_model(
      mcmc = create_mcmc(
        screenlog = create_screenlog(
          mode = "compound"
        )
      )
    )
  )
  expect_true(
    !is.na(
      stringr::str_match(xml, pattern = "mode=\"compound\"")[1, 1]
    )
  )
})
