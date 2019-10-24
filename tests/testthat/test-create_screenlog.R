test_that("use", {
  expect_silent(
    create_screenlog(
      filename = "my.trees",
      log_every = 1234,
      mode = "autodetect",
      sort = "alphabetic",
      sanitise_headers = TRUE
    )
  )
})
