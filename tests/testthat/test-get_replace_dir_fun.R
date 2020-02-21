test_that("minimal use", {
  expect_silent(get_replace_dir_fun())
  expect_silent(get_replace_dir_fun("~/"))
  expect_silent(get_replace_dir_fun("~"))
  expect_silent(get_replace_dir_fun("/home/john"))
})

test_that("correct behaviour", {
  f <- get_replace_dir_fun("~")
  expect_equal(f(NA), NA)
  expect_equal(f(""), "")
  expect_equal(
    f("/home/richel/.cache/beast2_186c7404208c.xml.state"),
    "~/beast2_186c7404208c.xml.state"
  )
  f <- get_replace_dir_fun("~/")
  expect_equal(
    f("/home/richel/.cache/beast2_186c7404208c.xml.state"),
    "~/beast2_186c7404208c.xml.state"
  )
  f <- get_replace_dir_fun("/home/john")
  expect_equal(
    f("/home/richel/.cache/beast2_186c7404208c.xml.state"),
    "/home/john/beast2_186c7404208c.xml.state"
  )
  expect_equal(
    f("hello.txt"),
    "/home/john/hello.txt"
  )
  expect_error(f(c()))
})
