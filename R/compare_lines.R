#' Internal debug function to compare the actually created
#' lines to expected lines.
compare_lines <- function(lines, expected) {

  write.csv(lines, "~/created.csv")
  write.csv(expected, "~/expected.csv")
  for (i in 1:min(length(expected), length(lines))) {
    testthat::expect_equal(
      expected[i], lines[i]
    )
    print(paste0(i, " / ", length(expected)))
  }
}
