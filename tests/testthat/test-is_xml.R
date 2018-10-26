context("is_xml")

test_that("use", {

  expect_false(is_xml(NA))
  expect_true(is_xml("<text content=\"Hello\"/>"))
  expect_true(is_xml(
    parameter_to_xml(create_param_alpha(id = 1)))
  )
  expect_true(is_xml(
    distr_to_xml(create_one_div_x_distr(id = 1)))
  )
  expect_false(is_xml("no opening tag"))
  expect_false(is_xml("<text id=1></invalid_close>"))
  expect_false(is_xml("<text content=\"Hello\">"))
})
