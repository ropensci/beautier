test_that("identicals", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  expect_true(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("identicals, same NA filename", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$filename <- NA
  screenlog_2$filename <- NA
  expect_true(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("identicals, same non-NA filename", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$filename <- "some.txt"
  screenlog_2$filename <- "some.txt"
  expect_true(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("filename, one NA", {

  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$filename <- NA
  screenlog_2$filename <- "something"
  expect_false(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("filename, no NA", {

  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$filename <- "something"
  screenlog_2$filename <- "else"
  expect_false(are_equal_screenlogs(screenlog_1, screenlog_2))
})


test_that("log_every", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$log_every <- 1234
  screenlog_2$log_every <- 5678
  expect_false(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("mode", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$mode <- "autodetect"
  screenlog_2$mode <- "compound"
  expect_false(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("sanitise_headers", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$sanitise_headers <- TRUE
  screenlog_2$sanitise_headers <- FALSE
  expect_false(are_equal_screenlogs(screenlog_1, screenlog_2))
})

test_that("sort", {
  screenlog_1 <- create_screenlog()
  screenlog_2 <- screenlog_1
  screenlog_1$sort <- "alphabetic"
  screenlog_2$sort <- "smart"
  expect_false(are_equal_screenlogs(screenlog_1, screenlog_2))
})
