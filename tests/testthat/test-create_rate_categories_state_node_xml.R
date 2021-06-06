test_that("minimal use", {
  expect_silent(
    create_rate_categories_state_node_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(
          id = 314,
          dimension = 1
        )
      )
    )
  )
})

test_that("detailed use", {
  expect_equal(
    create_rate_categories_state_node_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(
          id = 314,
          dimension = 1
        )
      )
    ),
    "<stateNode id=\"rateCategories.c:314\" spec=\"parameter.IntegerParameter\" dimension=\"1\">1</stateNode>" # nolint indeed a long line
  )
})

test_that("abuse", {
  expect_error(
    create_rate_categories_state_node_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(
          id = NA, # which it is by default
          dimension = 1
        )
      )
    ),
    "is_one_na\\(id\\)"
  )
  expect_error(
    create_rate_categories_state_node_xml(
      create_inference_model(
        clock_model = create_rln_clock_model(
          id = 1
        ) # dimension is NA
      )
    ),
    "is_one_na\\(dimension\\)"
  )
})
