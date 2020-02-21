test_that("replace dir from local to folder", {

  inference_model <- create_inference_model()
  inference_model$mcmc$tracelog$filename <- "trace.log"
  inference_model$mcmc$screenlog$filename <- "screen.log"
  inference_model$mcmc$treelog$filename <- "tree.log"
  inference_model$tipdates_filename <- "tipdates.csv"

  inference_model <- rename_inference_model_filenames(
    inference_model = inference_model,
    rename_fun = get_replace_dir_fun("/home/john")
  )

  expect_equal(inference_model$mcmc$tracelog$filename, "/home/john/trace.log")
  expect_equal(inference_model$mcmc$screenlog$filename, "/home/john/screen.log")
  expect_equal(inference_model$mcmc$treelog$filename, "/home/john/tree.log")
  expect_equal(inference_model$tipdates_filename, "/home/john/tipdates.csv")
})

test_that("rename dir from folder to folder", {

  inference_model <- create_inference_model()
  inference_model$mcmc$tracelog$filename <- "/home/john/trace.log"
  inference_model$mcmc$screenlog$filename <- "/home/john/screen.log"
  inference_model$mcmc$treelog$filename <- "/home/john/tree.log"
  inference_model$tipdates_filename <- "/home/john/tipdates.csv"

  inference_model <- rename_inference_model_filenames(
    inference_model = inference_model,
    rename_fun = get_replace_dir_fun("/home/doe")
  )

  expect_equal(inference_model$mcmc$tracelog$filename, "/home/doe/trace.log")
  expect_equal(inference_model$mcmc$screenlog$filename, "/home/doe/screen.log")
  expect_equal(inference_model$mcmc$treelog$filename, "/home/doe/tree.log")
  expect_equal(inference_model$tipdates_filename, "/home/doe/tipdates.csv")
})


test_that("remove dir", {

  inference_model <- create_inference_model()
  inference_model$mcmc$tracelog$filename <- "/home/john/trace.log"
  inference_model$mcmc$screenlog$filename <- "/home/john/screen.log"
  inference_model$mcmc$treelog$filename <- "/home/john/tree.log"
  inference_model$tipdates_filename <- "/home/john/tipdates.csv"

  inference_model <- rename_inference_model_filenames(
    inference_model = inference_model,
    rename_fun = get_remove_dir_fun()
  )

  expect_equal(inference_model$mcmc$tracelog$filename, "trace.log")
  expect_equal(inference_model$mcmc$screenlog$filename, "screen.log")
  expect_equal(inference_model$mcmc$treelog$filename, "tree.log")
  expect_equal(inference_model$tipdates_filename, "tipdates.csv")
})

test_that("use", {

  expect_silent(
    rename_inference_model_filenames(
      inference_model = create_inference_model(),
      rename_fun = get_remove_dir_fun()
    )
  )

  suppressMessages(get_replace_dir_fun())

  expect_silent(
    rename_inference_model_filenames(
      inference_model = create_inference_model(),
      rename_fun = get_replace_dir_fun()
    )
  )
  expect_error(
    rename_inference_model_filenames(
      inference_model = create_inference_model(),
      rename_fun = "nonsense"
    )
  )
})
