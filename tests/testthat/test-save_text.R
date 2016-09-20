context("save_text")

test_that("save_text: use", {
  filename <- tempfile()
  text <- c("Hello", "world")
  save_text(
    filename = filename,
    text = text
  )
  expect_equal(file.exists(filename), TRUE)

  # Remove temporary file
  has_removed <- file.remove(filename)
  expect_equal(file.exists(filename), FALSE)
})
