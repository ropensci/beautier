test_that("Package must pass lintr", {
  skip("Do this close to release")
  if (requireNamespace("lintr", quietly = TRUE)) {
    context("lints")
    test_that("Package Style", {
      lintr::expect_lint_free()
    })
  }
})
