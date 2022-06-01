#' Determines if the name is a valid parameter name
#' @param name the name to be tested
#' @return TRUE if the name is a valid parameter name, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_param_name("alpha")
#' is_param_name("beta")
#' is_param_name("clock_rate")
#' is_param_name("kappa_1")
#' is_param_name("kappa_2")
#' is_param_name("lambda")
#' is_param_name("m")
#' is_param_name("mean")
#' is_param_name("mu")
#' is_param_name("rate_ac")
#' is_param_name("rate_ag")
#' is_param_name("rate_at")
#' is_param_name("rate_cg")
#' is_param_name("rate_ct")
#' is_param_name("rate_gt")
#' is_param_name("s")
#' is_param_name("scale")
#' is_param_name("sigma")
#'
#' # FALSE
#' is_param_name("nonsense")
#' is_param_name(NA)
#' is_param_name(NULL)
#' is_param_name("")
#' is_param_name(c())
#'
#' check_empty_beautier_folder()
#' @export
is_param_name <- function(name) {
  if (length(name) == 0) return(FALSE)
  name %in% beautier::get_param_names()
}
