test_that("identicals", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  expect_true(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("identicals, same NA filename", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$filename <- NA
  treelog_2$filename <- NA
  expect_true(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("identicals, same non-NA filename", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$filename <- "some.txt"
  treelog_2$filename <- "some.txt"
  expect_true(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("filename, one NA", {

  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$filename <- NA
  treelog_2$filename <- "something"
  expect_false(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("filename, no NA", {

  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$filename <- "something"
  treelog_2$filename <- "else"
  expect_false(are_equal_treelogs(treelog_1, treelog_2))
})


test_that("log_every", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$log_every <- 1234
  treelog_2$log_every <- 5678
  expect_false(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("mode", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$mode <- "autodetect"
  treelog_2$mode <- "compound"
  expect_false(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("sanitise_headers", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$sanitise_headers <- TRUE
  treelog_2$sanitise_headers <- FALSE
  expect_false(are_equal_treelogs(treelog_1, treelog_2))
})

test_that("sort", {
  treelog_1 <- create_treelog()
  treelog_2 <- treelog_1
  treelog_1$sort <- "alphabetic"
  treelog_2$sort <- "smart"
  expect_false(are_equal_treelogs(treelog_1, treelog_2))
})
