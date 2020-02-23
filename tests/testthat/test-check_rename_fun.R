test_that("use", {
  # 'get_replace_dir_fun' triggers the call to 'pryr',
  # resulting in this message:
  #
  #    Registered S3 method overwritten by 'pryr':
  #    method      from
  #    print.bytes Rcpp.
  #
  # Therefore, first suppress that message, before expecting
  # 'get_replace_dir_fun' to be silent
  suppressMessages(
    check_rename_fun(get_replace_dir_fun())
  )

  expect_silent(check_rename_fun(get_remove_dir_fun()))
  expect_silent(check_rename_fun(get_remove_hex_fun()))
  expect_silent(check_rename_fun(get_replace_dir_fun()))
})

test_that("abuse", {

  # Data type: one function
  expect_error(
    check_rename_fun("nonsense"),
    "'rename_fun' must be one function"
  )
  expect_error(
    check_rename_fun(c()),
    "'rename_fun' must be one function"
  )
  expect_error(
    check_rename_fun(NA),
    "'rename_fun' must be one function"
  )
  expect_error(
    check_rename_fun(NULL),
    "'rename_fun' must be one function"
  )
  expect_error(
    check_rename_fun(Inf),
    "'rename_fun' must be one function"
  )
  expect_error(
    check_rename_fun(list(get_remove_dir_fun(), get_remove_dir_fun())),
    "'rename_fun' must be one function"
  )

  # Must return NA when given NA
  expect_error(
    check_rename_fun(
      function(filename) "something"
    ),
    "'rename_fun' must return NA when given an NA"
  )

  # Must return a character vector with one element
  expect_error(
    check_rename_fun(
      function(filename) NA
    ),
    "'rename_fun' must return a character vector with one element"
  )

  expect_error(
    check_rename_fun(
      function(filename) {
        if (is.na(filename)) return(NA)
        # Return two elements
        c(filename, filename)
      }
    ),
    "'rename_fun' must return a character vector with one element"
  )
})
