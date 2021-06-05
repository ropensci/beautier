test_that("minimal use", {
  expect_silent(clock_rate_param_to_xml(create_clock_rate_param(id = 1)))
  expect_silent(clock_rate_param_to_xml(create_clock_rate_param(id = 1, estimate = TRUE)))
  expect_silent(clock_rate_param_to_xml(create_clock_rate_param(id = 1, value = 12.34)))
})

test_that("v2.4", {
  expect_equal(
    clock_rate_param_to_xml(
      create_clock_rate_param(id = 1),
      beauti_options = create_beauti_options_v2_4()
    ),
    "<parameter id=\"clockRate.c:1\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>"
  )
})

test_that("v2.6", {
  expect_equal(
    clock_rate_param_to_xml(
      create_clock_rate_param(id = 1),
      beauti_options = create_beauti_options_v2_6()
    ),
    "<parameter id=\"clockRate.c:1\" spec=\"parameter.RealParameter\" estimate=\"false\" name=\"clock.rate\">1.0</parameter>"
  )
})

test_that("id", {
  expect_true(
    stringr::str_detect(
      clock_rate_param_to_xml(create_clock_rate_param(id = 314)),
      "314"
    )
  )
})

test_that("estimate", {
  expect_true(
    stringr::str_detect(
      clock_rate_param_to_xml(create_clock_rate_param(id = 1, estimate = TRUE)),
      "estimate=\"true\""
    )
  )
  expect_true(
    stringr::str_detect(
      clock_rate_param_to_xml(create_clock_rate_param(id = 1, estimate = FALSE)),
      "estimate=\"false\""
    )
  )
})

test_that("value", {
  expect_true(
    stringr::str_detect(
      clock_rate_param_to_xml(create_clock_rate_param(id = 1, value = "3.14")),
      ">3.14<"
    )
  )
})
