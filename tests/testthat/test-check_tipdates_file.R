test_that("use", {

  tipdates_filename <- get_beautier_path("babette_issue_109.tsv")
  expect_silent(check_tipdates_file(tipdates_filename))

  tipdates_filename <- get_beautier_path(
    "babette_issue_109_no_tabs.tsv"
  )
  expect_error(
    check_tipdates_file(tipdates_filename),
    "is not a tab-separated file"
  )

  tipdates_filename <- get_beautier_path(
    "babette_issue_109_with_header.tsv"
  )
  expect_error(check_tipdates_file(tipdates_filename), "has a header")


})
