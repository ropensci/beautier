test_that("use", {
  created <- create_xml_declaration()
  expected <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
  expect_equal(created, expected)
})
