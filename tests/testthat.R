library(testthat)
library(beautier)

remove_beautier_folder()
check_empty_beautier_folder()

test_check("beautier")

check_empty_beautier_folder()
remove_beautier_folder()
