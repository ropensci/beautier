library(testthat)
library(beautier)


unlink(get_beautier_folder(), recursive = TRUE)
check_empty_beautier_folder()

test_check("beautier")

check_empty_beautier_folder()
