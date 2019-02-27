#' Determine if the object is a valid parameter
#' @param x an object, to be determined if it is a valid parameter,
#'   as created by \code{\link{create_param}})
#' @return TRUE if x is a valid parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_param <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!x$name %in% get_param_names()) return(FALSE) # nolint beautier function
  if (!"id" %in% names(x)) return(FALSE)
  if (!"value" %in% names(x)) return(FALSE)
  if (is_one_na(x$value)) return(FALSE) # nolint beautier function
  TRUE
}

#' Determine if the object is a valid
#' alpha parameter
#' @param x an object, to be determined if it is a valid
#'   alpha parameter
#' @return TRUE if x is a valid alpha parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_alpha_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "alpha"
}

#' Determine if the object is a valid
#' beta parameter
#' @param x an object, to be determined if it is a valid
#'   beta parameter
#' @return TRUE if x is a valid beta parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_beta_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "beta"
}

#' Determine if the object is a valid
#' clock_rate parameter
#' @param x an object, to be determined if it is a valid
#'   clock_rate parameter
#' @return TRUE if x is a valid clock_rate parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_clock_rate_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "clock_rate"
}

#' Determine if the object is a valid kappa 1 parameter
#' @param x an object, to be determined if it is a valid
#'   kappa 1 parameter
#' @return TRUE if x is a valid kappa 1 parameter,
#'   FALSE otherwise
#' @seealso kappa 1 parameters are returned by
#'   \code{\link{create_kappa_1_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   kappa_1_param <- create_kappa_1_param()
#'   testit::assert(beautier:::is_kappa_1_param(kappa_1_param))
#' @noRd
is_kappa_1_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  if (x$name != "kappa_1") return(FALSE)

  if (!"lower" %in% names(x)) return(FALSE)
  if (!"estimate" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid kappa 2 parameter
#' @param x an object, to be determined if it is a valid
#'   kappa 2 parameter
#' @return TRUE if x is a valid kappa_2 parameter,
#'   FALSE otherwise
#' @seealso kappa 2 parameters are returned by
#'   \code{\link{create_kappa_2_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   kappa_2_param <- create_kappa_2_param()
#'   testit::assert(beautier:::is_kappa_2_param(kappa_2_param))
#' @noRd
is_kappa_2_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  if (x$name != "kappa_2") return(FALSE)

  if (!"lower" %in% names(x)) return(FALSE)
  if (!"estimate" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid lambda parameter
#' @param x an object, to be determined if it is a valid
#'   lambda parameter
#' @return TRUE if x is a valid lambda parameter,
#'   FALSE otherwise
#' @seealso lambda parameters are returned by \code{\link{create_lambda_param}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   lambda_param <- create_lambda_param()
#'   testit::assert(beautier:::is_lambda_param(lambda_param))
#' @noRd
is_lambda_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "lambda"
}

#' Determine if the object is a valid
#' m parameter
#' @param x an object, to be determined if it is a valid
#'   m parameter
#' @return TRUE if x is a valid m parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_m_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "m"
}

#' Determine if the object is a valid mean parameter
#' @param x an object, to be determined if it is a valid mean parameter,
#'   as created by \code{\link{create_mean_param}})
#' @return TRUE if x is a valid mean parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_mean_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "mean"
}

#' Determine if the object is a valid
#' mu parameter
#' @param x an object, to be determined if it is a valid
#'   mu parameter
#' @return TRUE if x is a valid mu parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_mu_param}} creates a mu parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   mu_param <- create_mu_param()
#'   testit::assert(beautier:::is_mu_param(mu_param))
#' @noRd
is_mu_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "mu"
}

#' Determine if the object is a valid
#' 'rate AC' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AC' parameter
#' @return TRUE if x is a valid 'rate AC' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ac_param}} creates a 'rate AC' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rate_ac_param <- create_rate_ac_param()
#'   testit::assert(beautier:::is_rate_ac_param(rate_ac_param))
#' @noRd
is_rate_ac_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "rate_ac"
}

#' Determine if the object is a valid
#' 'rate AG' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AG' parameter
#' @return TRUE if x is a valid 'rate AG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ag_param}} creates a 'rate AG' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rate_ag_param <- create_rate_ag_param()
#' @noRd
is_rate_ag_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "rate_ag"
}

#' Determine if the object is a valid
#' 'rate AT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate AT' parameter
#' @return TRUE if x is a valid 'rate AT' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_at_param}} creates a 'rate AT' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rate_at_param <- create_rate_at_param()
#'   testit::assert(beautier:::is_rate_at_param(rate_at_param))
#' @noRd
is_rate_at_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "rate_at"
}

#' Determine if the object is a valid
#' 'rate CG' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate CG' parameter
#' @return TRUE if x is a valid 'rate CG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_cg_param}} creates a 'rate CG' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rate_cg_param <- create_rate_cg_param()
#'   testit::assert(beautier:::is_rate_cg_param(rate_cg_param))
#' @noRd
is_rate_cg_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "rate_cg"
}

#' Determine if the object is a valid
#' 'rate CT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate CT' parameter
#' @return TRUE if x is a valid 'rate CG' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_ct_param}} creates a 'rate CT' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rate_ct_param <- create_rate_ct_param()
#'   testit::assert(beautier:::is_rate_ct_param(rate_ct_param))
#' @noRd
is_rate_ct_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "rate_ct"
}

#' Determine if the object is a valid
#' 'rate GT' parameter
#' @param x an object, to be determined if it is a valid
#'   'rate GT' parameter
#' @return TRUE if x is a valid 'rate GT' parameter,
#'   FALSE otherwise
#' @seealso \code{\link{create_rate_gt_param}} creates a 'rate GT' parameter
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   rate_gt_param <- create_rate_gt_param()
#'   testit::assert(beautier:::is_rate_gt_param(rate_gt_param))
#' @noRd
is_rate_gt_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "rate_gt"
}

#' Determine if the object is a valid
#' s parameter
#' @param x an object, to be determined if it is a valid
#'   s parameter
#' @return TRUE if x is a valid s parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_s_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "s"
}

#' Determine if the object is a valid
#' scale parameter
#' @param x an object, to be determined if it is a valid
#'   scale parameter
#' @return TRUE if x is a valid scale parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_scale_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "scale"
}

#' Determine if the object is a valid
#' sigma parameter
#' @param x an object, to be determined if it is a valid
#'   sigma parameter
#' @return TRUE if x is a valid sigma parameter,
#'   FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_sigma_param <- function(
  x
) {
  if (!is_param(x)) return(FALSE) # nolint beautier function
  x$name == "sigma"
}
