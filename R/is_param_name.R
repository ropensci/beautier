#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   testit::assert(beautier:::is_param_name("alpha"))
#'   testit::assert(beautier:::is_param_name("beta"))
#'   testit::assert(beautier:::is_param_name("clock_rate"))
#'   testit::assert(beautier:::is_param_name("kappa_1"))
#'   testit::assert(beautier:::is_param_name("kappa_2"))
#'   testit::assert(beautier:::is_param_name("lambda"))
#'   testit::assert(beautier:::is_param_name("m"))
#'   testit::assert(beautier:::is_param_name("mean"))
#'   testit::assert(beautier:::is_param_name("mu"))
#'   testit::assert(beautier:::is_param_name("rate_ac"))
#'   testit::assert(beautier:::is_param_name("rate_ag"))
#'   testit::assert(beautier:::is_param_name("rate_at"))
#'   testit::assert(beautier:::is_param_name("rate_cg"))
#'   testit::assert(beautier:::is_param_name("rate_ct"))
#'   testit::assert(beautier:::is_param_name("rate_gt"))
#'   testit::assert(beautier:::is_param_name("s"))
#'   testit::assert(beautier:::is_param_name("scale"))
#'   testit::assert(beautier:::is_param_name("sigma"))
#' @noRd
is_param_name <- function(name) {
  name %in% get_param_names() # nolint beautier function
}
