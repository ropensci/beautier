test_that("use", {
  expect_silent(
    check_treelog(
      create_treelog()
    )
  )

  expect_silent(
    check_treelog(
      create_treelog(
        filename = "something"
      )
    )
  )
  expect_silent(
    check_treelog(
      create_treelog(
        log_every = 1234
      )
    )
  )

})
