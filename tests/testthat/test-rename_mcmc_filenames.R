test_that("replace dir from local to folder", {

  mcmc <- create_mcmc()
  mcmc$tracelog$filename <- "trace.log"
  mcmc$screenlog$filename <- "screen.log"
  mcmc$treelog$filename <- "tree.log"

  mcmc <- rename_mcmc_filenames(
    mcmc = mcmc,
    rename_fun = get_replace_dir_fun("/home/john")
  )

  expect_equal(mcmc$tracelog$filename, "/home/john/trace.log")
  expect_equal(mcmc$screenlog$filename, "/home/john/screen.log")
  expect_equal(mcmc$treelog$filename, "/home/john/tree.log")
})

test_that("rename dir from folder to folder", {

  mcmc <- create_mcmc()
  mcmc$tracelog$filename <- "/home/john/trace.log"
  mcmc$screenlog$filename <- "/home/john/screen.log"
  mcmc$treelog$filename <- "/home/john/tree.log"

  mcmc <- rename_mcmc_filenames(
    mcmc = mcmc,
    rename_fun = get_replace_dir_fun("/home/doe")
  )

  expect_equal(mcmc$tracelog$filename, "/home/doe/trace.log")
  expect_equal(mcmc$screenlog$filename, "/home/doe/screen.log")
  expect_equal(mcmc$treelog$filename, "/home/doe/tree.log")
})


test_that("remove dir", {

  mcmc <- create_mcmc()
  mcmc$tracelog$filename <- "/home/john/trace.log"
  mcmc$screenlog$filename <- "/home/john/screen.log"
  mcmc$treelog$filename <- "/home/john/tree.log"

  mcmc <- rename_mcmc_filenames(
    mcmc = mcmc,
    rename_fun = get_remove_dir_fun()
  )

  expect_equal(mcmc$tracelog$filename, "trace.log")
  expect_equal(mcmc$screenlog$filename, "screen.log")
  expect_equal(mcmc$treelog$filename, "tree.log")
})

test_that("use", {

  expect_silent(
    rename_mcmc_filenames(
      mcmc = create_test_mcmc(),
      rename_fun = get_remove_dir_fun()
    )
  )

  suppressMessages(get_replace_dir_fun())

  expect_silent(
    rename_mcmc_filenames(
      mcmc = create_test_mcmc(),
      rename_fun = get_replace_dir_fun()
    )
  )
  expect_error(
    rename_mcmc_filenames(
      mcmc = create_test_mcmc(),
      rename_fun = "nonsense"
    )
  )
})
