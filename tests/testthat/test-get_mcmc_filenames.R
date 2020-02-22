test_that("use, all three filenames", {

  mcmc <- create_mcmc()
  mcmc$tracelog$filename <- "/home/john/trace.log"
  mcmc$screenlog$filename <- "/home/john/screen.log"
  mcmc$treelog$filename <- "/home/john/tree.log"

  filenames <- get_mcmc_filenames(mcmc)

  expect_true("/home/john/trace.log" %in% filenames)
  expect_true("/home/john/screen.log" %in% filenames)
  expect_true("/home/john/tree.log" %in% filenames)
})

test_that("no screenlog is no filename", {

  mcmc <- create_mcmc()
  mcmc$tracelog$filename <- "/home/john/trace.log"
  mcmc$screenlog$filename <- ""
  mcmc$treelog$filename <- "/home/john/tree.log"

  filenames <- get_mcmc_filenames(mcmc)

  expect_equal(length(filenames), 2)
  expect_true("/home/doe/trace.log" %in% filenames)
  expect_true(!"" %in% filenames)
  expect_true("/home/doe/tree.log" %in% filenames)
})

test_that("use", {

  expect_error(
    get_mcmc_filenames("nonsense")
  )
})
