test_that("Package style", {
  # Only test on Travis CI
  if (Sys.getenv("TRAVIS") != "") {
    lintr::expect_lint_free()
  }
})
