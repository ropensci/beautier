test_that("use", {
  expect_true(are_ids("anthus_aco"))
  expect_true(are_ids(c("anthus_aco", "anthus_nd2")))
  expect_true(are_ids(list("anthus_aco", "anthus_nd2")))
  expect_true(are_ids(c(1, 2)))
  expect_true(are_ids(1))
  expect_false(are_ids(NULL))
  expect_false(are_ids(NA))
  expect_false(are_ids(c()))
  expect_false(are_ids(ape::rcoal(3)))
  expect_false(are_ids(c(ape::rcoal(3), ape::rcoal(4))))

})
