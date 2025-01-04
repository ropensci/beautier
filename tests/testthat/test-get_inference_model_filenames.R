test_that("use, all three filenames", {

  inference_model <- create_inference_model()
  inference_model$mcmc$tracelog$filename <- "/home/john/trace.log"
  inference_model$mcmc$screenlog$filename <- "/home/john/screen.log"
  inference_model$mcmc$treelog$filename <- "/home/john/tree.log"
  inference_model$tipdates_filename <- get_beautier_path(
    "babette_issue_108_tipdates.txt"
  )

  filenames <- get_inference_model_filenames(inference_model)

  expect_true("/home/john/trace.log" %in% filenames)
  expect_true("/home/john/screen.log" %in% filenames)
  expect_true("/home/john/tree.log" %in% filenames)
  expect_true(inference_model$tipdates_filename %in% filenames)
})

test_that("no tipdates filename", {

  inference_model <- create_inference_model()
  inference_model$mcmc$tracelog$filename <- "/home/john/trace.log"
  inference_model$mcmc$screenlog$filename <- "/home/john/screen.log"
  inference_model$mcmc$treelog$filename <- "/home/john/tree.log"
  inference_model$tipdates_filename <- NA

  filenames <- get_inference_model_filenames(inference_model)

  expect_equal(length(filenames), 3)
  expect_true("/home/john/trace.log" %in% filenames)
  expect_true("/home/john/screen.log" %in% filenames)
  expect_true("/home/john/tree.log" %in% filenames)
})

test_that("use", {

  expect_error(
    get_inference_model_filenames("nonsense")
  )
})
