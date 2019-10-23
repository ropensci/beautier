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

test_that("abuse", {
  expect_error(
    check_treelog(
      create_treelog(
        filename = ""
      )
    ),
    "filename"
  )
  expect_error(
    check_treelog(
      create_treelog(
        filename = 1234
      )
    ),
    "filename"
  )

  expect_error(
    check_treelog(
      create_treelog(
        log_every = "nonsense"
      )
    ),
    "log_every"
  )
  expect_error(
    check_treelog(
      create_treelog(
        log_every = -1234
      )
    ),
    "log_every"
  )
})
