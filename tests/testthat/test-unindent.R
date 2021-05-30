test_that("no text", {
  text <- c()
  expect_equal(unindent(text), c())
})

test_that("no text", {
  text <- c("")
  expect_equal(unindent(text), "")
})

test_that("unindented, 1 line", {
  text <- c("x")
  expect_equal(unindent(text), "x")
})

test_that("indented, 1 line", {
  text <- c("   x")
  expect_equal(unindent(text), "x")
})

test_that("unindented, 3 line", {
  text <- c(
    "<tag>",
    "  stuff",
    "</tag>"
  )
  expect_equal(unindent(text), text)
})

test_that("indented, 3 line", {
  text <- c(
    "  <tag>",
    "    stuff",
    "  </tag>"
  )
  expect_equal(unindent(text)[1], "<tag>")
  expect_equal(unindent(text)[2], "  stuff")
  expect_equal(unindent(text)[3], "</tag>")
})
