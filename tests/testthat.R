library(testthat)
library(beautier)

test_check("beautier")

testthat::expect_equal(
  0,
  length(list.files(rappdirs::user_cache_dir(appname = "beautier")))
)
