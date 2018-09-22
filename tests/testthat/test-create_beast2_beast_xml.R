context("create_beast2_beautitemplate_xml")

test_that("use", {

  created_2_4 <- create_beast2_beast_xml(beast2_version = "2.4")
  created_2_5 <- create_beast2_beast_xml(beast2_version = "2.5")
  expect_true(created_2_4 != created_2_5)
})
