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
  expect_silent(
    check_treelog(
      create_treelog(
        sanitise_headers = TRUE
      )
    )
  )

})

test_that("abuse, by removing elements", {
  good_treelog <- create_treelog()

  treelog <- good_treelog
  treelog$filename <- NULL
  expect_error(check_treelog(treelog), "filename")

  treelog <- good_treelog
  treelog$log_every <- NULL
  expect_error(check_treelog(treelog), "log_every")

  treelog <- good_treelog
  treelog$mode <- NULL
  expect_error(check_treelog(treelog), "mode")

  treelog <- good_treelog
  treelog$sanitise_headers <- NULL
  expect_error(check_treelog(treelog), "sanitise_headers")

  treelog <- good_treelog
  treelog$sort <- NULL
  expect_error(check_treelog(treelog), "sort")
})

test_that("abuse, by wrong values", {
  expect_error(
    check_treelog(
      create_treelog(
        filename = 1234
      )
    )
  )

  expect_error(
    check_treelog(
      create_treelog(
        filename = "filename with spaces.csv"
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

  expect_error(
    check_treelog(
      create_treelog(
        mode = "nonsense"
      )
    ),
    "mode"
  )

  expect_error(
    check_treelog(
      create_treelog(
        sanitise_headers = "nonsense"
      )
    ),
    "sanitise_headers"
  )

  expect_error(
    check_treelog(
      create_treelog(
        sort = "nonsense"
      )
    ),
    "sort"
  )

})
