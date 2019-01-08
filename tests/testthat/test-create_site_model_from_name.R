context("test-create_site_model_from_name")

test_that("use", {
  names <- c("JC69", "HKY", "TN93", "GTR")
  for (name in names) {
    expect_equal(name, create_site_model_from_name(name)$name)
  }
})
