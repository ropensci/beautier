context("create_random_alignment")

test_that("use", {

  testthat::expect_silent(
    beautier:::create_random_alignment(
      n_taxa = 5,
      sequence_length = 20,
      rate = 0.1
    )
  )

  testthat::expect_silent(
    beautier:::create_random_alignment(
      n_taxa = 5,
      sequence_length = 20,
      rate = 0.1,
      taxa_name_ext = "_aco"
    )
  )

})

test_that("checks input", {
  testthat::expect_silent(
    create_random_alignment(
      n_taxa = 5,
      sequence_length = 20,
      rate = 0.1
    )
  )

  expect_error(
    create_random_alignment(
      n_taxa = 0, #Error
      sequence_length = 20,
      rate = 0.1
    )
  )

  expect_error(
    create_random_alignment(
      n_taxa = 5,
      sequence_length = 0, # Error
      rate = 0.1
    )
  )

  expect_error(
    create_random_alignment(
      n_taxa = 5,
      sequence_length = 20,
      rate = -0.1 #Error
    )
  )

  expect_error(
    create_random_alignment(
      n_taxa = 5,
      sequence_length = 20,
      rate = 1.1 # Error
    )
  )

  result <- create_random_alignment(
      n_taxa = 5,
      sequence_length = 20,
      rate = 0.1
    )
  expect_equal(class(result), "DNAbin")
})
