test_that("use", {
  tipdates_filename <- get_beautier_path("babette_issue_109.tsv")
  t <- read_tipdates_file(tipdates_filename)
  expect_equal(nrow(t), 6)
  expect_equal(ncol(t), 2)
  expect_equal(t$time[1], "2013.25")
})

test_that("use strings for taxon", {
  tipdates_filename <- get_beautier_path("G_VII_pre2003_dates_4.txt")
  readLines(get_beautier_path("G_VII_pre2003_dates_4.txt"))
  t <- read_tipdates_file(tipdates_filename)
  expect_equal(nrow(t), 58)
  expect_equal(ncol(t), 2)
  expect_equal(t$time[1], "1976")
})
