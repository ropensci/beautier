#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_true(is_param_name("alpha"))
#' expect_true(is_param_name("beta"))
#' expect_true(is_param_name("clock_rate"))
#' expect_true(is_param_name("kappa_1"))
#' expect_true(is_param_name("kappa_2"))
#' expect_true(is_param_name("lambda"))
#' expect_true(is_param_name("m"))
#' expect_true(is_param_name("mean"))
#' expect_true(is_param_name("mu"))
#' expect_true(is_param_name("rate_ac"))
#' expect_true(is_param_name("rate_ag"))
#' expect_true(is_param_name("rate_at"))
#' expect_true(is_param_name("rate_cg"))
#' expect_true(is_param_name("rate_ct"))
#' expect_true(is_param_name("rate_gt"))
#' expect_true(is_param_name("s"))
#' expect_true(is_param_name("scale"))
#' expect_true(is_param_name("sigma"))
#'
#' expect_false(is_param_name("nonsense"))
#' expect_false(is_param_name(NA))
#' expect_false(is_param_name(NULL))
#' expect_false(is_param_name(""))
#' expect_false(is_param_name(c()))
#' @export
is_param_name <- function(name) {
  if (length(name) == 0) return(FALSE)
  name %in% beautier::get_param_names()
}
