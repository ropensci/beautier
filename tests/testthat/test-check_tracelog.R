test_that("use", {
  expect_silent(
    check_tracelog(
      create_tracelog()
    )
  )

  expect_silent(
    check_tracelog(
      create_tracelog(
        filename = "something"
      )
    )
  )
  expect_silent(
    check_tracelog(
      create_tracelog(
        log_every = 1234
      )
    )
  )
  expect_silent(
    check_tracelog(
      create_tracelog(
        sanitise_headers = TRUE
      )
    )
  )

})

test_that("abuse, by removing elements", {
  good_tracelog <- create_tracelog()

  tracelog <- good_tracelog
  tracelog$filename <- NULL
  expect_error(check_tracelog(tracelog), "filename")

  tracelog <- good_tracelog
  tracelog$log_every <- NULL
  expect_error(check_tracelog(tracelog), "log_every")

  tracelog <- good_tracelog
  tracelog$mode <- NULL
  expect_error(check_tracelog(tracelog), "mode")

  tracelog <- good_tracelog
  tracelog$sanitise_headers <- NULL
  expect_error(check_tracelog(tracelog), "sanitise_headers")

  tracelog <- good_tracelog
  tracelog$sort <- NULL
  expect_error(check_tracelog(tracelog), "sort")
})

test_that("abuse, by wrong values", {
  expect_error(
    check_tracelog(
      create_tracelog(
        filename = 1234
      )
    ),
    "filename"
  )
  expect_error(
    check_tracelog(
      create_tracelog(
        filename = "filename with spaces.csv"
      )
    ),
    "filename"
  )
  expect_error(
    check_tracelog(
      create_tracelog(
        filename = 1234
      )
    ),
    "filename"
  )

  expect_error(
    check_tracelog(
      create_tracelog(
        log_every = "nonsense"
      )
    ),
    "log_every"
  )
  expect_error(
    check_tracelog(
      create_tracelog(
        log_every = -1234
      )
    ),
    "log_every"
  )

  expect_error(
    check_tracelog(
      create_tracelog(
        mode = "nonsense"
      )
    ),
    "mode"
  )

  expect_error(
    check_tracelog(
      create_tracelog(
        sanitise_headers = "nonsense"
      )
    ),
    "sanitise_headers"
  )

  expect_error(
    check_tracelog(
      create_tracelog(
        sort = "nonsense"
      )
    ),
    "sort"
  )
})
