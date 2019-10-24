test_that("use", {
  expect_silent(
    create_tracelog(
      filename = "my.trees",
      log_every = 1234,
      mode = "compound",
      sort = "alphabetic",
      sanitise_headers = TRUE
    )
  )
})
