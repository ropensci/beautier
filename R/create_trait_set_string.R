#' Create a trait set string.
#'
#' For example, a data frame with row \code{A 1}
#' and another row \code{B 2}, the trait set string will be
#' \code{A=1,B=2}
#' @param df a data frame with two columns
#' @return the trait set string
#' @author Richèl J.C. Bilderbeek
#' @export
create_trait_set_string <- function(
  df
) {
  check_true(is.data.frame(df))
  check_true(ncol(df) == 2)
  str <- NULL
  n_rows <- nrow(df)
  for (i in seq(1, n_rows)) {
    line <- paste0(as.character(df[i, 1]), "=", as.character(df[i, 2]), ",")
    str <- paste0(str, line)
  }
  # Remove last comma
  substr(str, 1, nchar(str) - 1)
}
