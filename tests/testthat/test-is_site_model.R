context("is_site_model")

test_that("use", {

  expect_true(is_site_model(create_jc69_site_model()))

  expect_false(is_site_model(NA))
  expect_false(is_site_model(NULL))
  expect_false(is_site_model("nonsense"))
  expect_false(is_site_model(list(name = "nonsense")))
  expect_false(is_site_model(list(name = "JC69")))

})

test_that("devious", {

  g <- create_jc69_site_model()
  expect_true(is_site_model(g))


  # No 'name'
  h <- g[names(g) != "name"]
  expect_false(is_site_model(h))

  # Invald 'name'
  h <- g
  h$name <- "nonsense"
  expect_false(is_site_model(h))

  # No 'id'
  h <- g[names(g) != "id"]
  expect_false(is_site_model(h))

  # No 'gamma_site_model'
  h <- g[names(g) != "gamma_site_model"]
  expect_false(is_site_model(h))

  # Invalid 'gamma_site_model'
  h <- g
  h$gamma_site_model <- "nonsense"
  expect_false(is_site_model(h))
})
