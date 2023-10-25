#' Determine if x is an initialized clock_model object,
#' as created by \code{\link{create_clock_model}}
#' @param x the object to check if it is an
#'   initialized clock_models object
#' @return TRUE if x is an initialized clock_model object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_clock_model <- function(
  x
) {
  if (!is_clock_model(x)) return(FALSE)
  if (is_strict_clock_model(x)) {
    return(is_init_strict_clock_model(x))
  } else {
    check_true(is_rln_clock_model(x))
    return(is_init_rln_clock_model(x))
  }
}

#' Determine if x is an initialized relaxed log-normal clock_model object
#' @inheritParams default_params_doc
#' @return TRUE if x is an initialized relaxed log-normal clock_model object,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_rln_clock_model <- function(
  rln_clock_model
) {
  check_true(is_rln_clock_model(rln_clock_model))
  if (!is_init_distr(rln_clock_model$ucldstdev_distr)) return(FALSE)
  if (!is_init_distr(rln_clock_model$mean_rate_prior_distr)) {
    return(FALSE)
  }
  !is_one_na(rln_clock_model$mparam_id) &&
    !is_one_na(rln_clock_model$dimension)
}

#' Determine if x is an initialized strict clock_model object
#' @inheritParams default_params_doc
#' @return TRUE if x is an initialized strict clock_model object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_strict_clock_model <- function(
  strict_clock_model
) {
  check_true(is_strict_clock_model(strict_clock_model))
  if (!is_init_distr(strict_clock_model$clock_rate_distr)) {
    return(FALSE)
  }
  !is_one_na(strict_clock_model$clock_rate_param$id)
}
