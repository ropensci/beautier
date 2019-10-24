test_that("use", {
  expect_silent(
    check_screenlog(
      create_screenlog()
    )
  )

  expect_silent(
    check_screenlog(
      create_screenlog(
        filename = "something"
      )
    )
  )
  expect_silent(
    check_screenlog(
      create_screenlog(
        log_every = 1234
      )
    )
  )
  expect_silent(
    check_screenlog(
      create_screenlog(
        sanitise_headers = TRUE
      )
    )
  )

})

test_that("abuse, by removing elements", {
  good_screenlog <- create_screenlog()

  screenlog <- good_screenlog
  screenlog$filename <- NULL
  expect_error(check_screenlog(screenlog), "filename")

  screenlog <- good_screenlog
  screenlog$log_every <- NULL
  expect_error(check_screenlog(screenlog), "log_every")

  screenlog <- good_screenlog
  screenlog$mode <- NULL
  expect_error(check_screenlog(screenlog), "mode")

  screenlog <- good_screenlog
  screenlog$sanitise_headers <- NULL
  expect_error(check_screenlog(screenlog), "sanitise_headers")

  screenlog <- good_screenlog
  screenlog$sort <- NULL
  expect_error(check_screenlog(screenlog), "sort")
})

test_that("abuse, by wrong values", {
  expect_error(
    check_screenlog(
      create_screenlog(
        filename = 1234
      )
    ),
    "filename"
  )
  expect_error(
    check_screenlog(
      create_screenlog(
        filename = 1234
      )
    ),
    "filename"
  )

  expect_error(
    check_screenlog(
      create_screenlog(
        log_every = "nonsense"
      )
    ),
    "log_every"
  )
  expect_error(
    check_screenlog(
      create_screenlog(
        log_every = -1234
      )
    ),
    "log_every"
  )

  expect_error(
    check_screenlog(
      create_screenlog(
        mode = "nonsense"
      )
    ),
    "mode"
  )

  expect_error(
    check_screenlog(
      create_screenlog(
        sanitise_headers = "nonsense"
      )
    ),
    "sanitizs_headers"
  )

  expect_error(
    check_screenlog(
      create_screenlog(
        sort = "nonsense"
      )
    ),
    "sort"
  )
})
