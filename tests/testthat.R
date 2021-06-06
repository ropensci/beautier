library(testthat)
library(beautier)

unlink(list.dirs(rappdirs::user_cache_dir(appname = "beautier")))

test_check("beautier")

testthat::expect_equal(
  0,
  length(list.files(rappdirs::user_cache_dir(appname = "beautier")))
)
