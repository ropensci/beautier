test_that("use", {
  expect_silent(m_param_to_xml(create_m_param(id = 1)))
  expect_silent(m_param_to_xml(create_m_param(id = 1, estimate = TRUE)))
  expect_silent(m_param_to_xml(create_m_param(id = 1, lower = 0.5)))
  expect_silent(m_param_to_xml(create_m_param(id = 1, upper = 2.0)))
  expect_silent(m_param_to_xml(create_m_param(id = 1, value = 1.2)))
})

test_that("use", {
  created <- m_param_to_xml(
    m_param = create_m_param(
      id = 1, estimate = FALSE, lower = "0.0", upper = "1.0", value = "1.0"
    )
  )
  expected <- "<parameter id=\"RealParameter.1\" estimate=\"false\" lower=\"0.0\" name=\"M\" upper=\"1.0\">1.0</parameter>" # nolint indeed long
  expect_equal(created, expected)
})


test_that("ID", {
  expect_true(
    stringr::str_detect(
      m_param_to_xml(create_m_param(id = "magic_id")),
      "id=\"RealParameter.magic_id\""
    )
  )
})

test_that("estimate", {
  expect_true(
    stringr::str_detect(
      m_param_to_xml(create_m_param(id = 1, estimate = FALSE)),
      "estimate=\"false\""
    )
  )
  expect_true(
    stringr::str_detect(
      m_param_to_xml(create_m_param(id = 1, estimate = TRUE)),
      "estimate=\"true\""
    )
  )
})



test_that("lower", {
  expect_true(
    stringr::str_detect(
      m_param_to_xml(create_m_param(id = 1, lower = "3.14")),
      "lower=\"3.14\"",
    )
  )
})

test_that("lower", {
  expect_true(
    stringr::str_detect(
      m_param_to_xml(create_m_param(id = 1, upper = "2.71")),
      "upper=\"2.71\"",
    )
  )
})
