#' Create a random name, for example,
#'   to be used by \code{\link{create_mrca_prior}}
#' @inheritParams default_params_doc
#' @return a random name
#' @author Richel J.C. Bilderbeek
create_random_name <- function() {
  paste0(sample(x = letters, size = 42, replace = TRUE), collapse = "")
}
