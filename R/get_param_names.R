#' Get the parameter names
#' @return the parameter names
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' names <- get_param_names()
#'
#' check_empty_beautier_folder()
#' @export
get_param_names <- function() {
  c(
    "alpha",
    "b_pop_sizes", # bPopSizes
    "beta",
    "clock_rate",
    "freqParameter", # BEAST2 redundant naming: 'freq' is the name of a parameter # nolint
    "lambda",
    "kappa",
    "kappa_1",
    "kappa_2",
    "m",
    "mean",
    "mu",
    "rate_ac",
    "rate_ag",
    "rate_at",
    "rate_cg",
    "rate_ct",
    "rate_gt",
    "s",
    "scale",
    "sigma"
  )
}
