test_that("identicals", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  expect_true(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("identicals, same NA filename", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$filename <- NA
  tracelog_2$filename <- NA
  expect_true(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("identicals, same non-NA filename", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$filename <- "some.txt"
  tracelog_2$filename <- "some.txt"
  expect_true(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("filename, one NA", {

  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$filename <- NA
  tracelog_2$filename <- "something"
  expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("filename, no NA", {

  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$filename <- "something"
  tracelog_2$filename <- "else"
  expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
})


test_that("log_every", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$log_every <- 1234
  tracelog_2$log_every <- 5678
  expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("mode", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$mode <- "autodetect"
  tracelog_2$mode <- "compound"
  expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("sanitise_headers", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$sanitise_headers <- TRUE
  tracelog_2$sanitise_headers <- FALSE
  expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
})

test_that("sort", {
  tracelog_1 <- create_tracelog()
  tracelog_2 <- tracelog_1
  tracelog_1$sort <- "alphabetic"
  tracelog_2$sort <- "smart"
  expect_false(are_equal_tracelogs(tracelog_1, tracelog_2))
})
