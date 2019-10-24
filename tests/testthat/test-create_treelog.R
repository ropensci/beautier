test_that("use", {
  expect_silent(
    create_treelog(
      filename = "my.trees",
      log_every = 1234,
      mode = "compound",
      sort = "smart",
      sanitise_headers = TRUE
    )
  )
})
