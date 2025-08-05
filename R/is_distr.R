#' Determine if the object is a valid distribution
#' @param x an object, to be determined if it is a valid
#'   distribution
#' @return TRUE if x is a valid distribution,
#'   FALSE otherwise
#' @seealso use
#'  \code{\link{is_beta_distr}},
#'  \code{\link{is_exp_distr}},
#'  \code{\link{is_gamma_distr}},
#'  \code{\link{is_inv_gamma_distr}},
#'  \code{\link{is_laplace_distr}},
#'  \code{\link{is_log_normal_distr}},
#'  \code{\link{is_normal_distr}},
#'  \code{\link{is_one_div_x_distr}},
#'  \code{\link{is_poisson_distr}},
#'  or \code{\link{is_uniform_distr}},
#'  to check for more specific distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_distr(create_beta_distr())
#' is_distr(create_exp_distr())
#' is_distr(create_gamma_distr())
#' is_distr(create_inv_gamma_distr())
#' is_distr(create_laplace_distr())
#' is_distr(create_log_normal_distr())
#' is_distr(create_normal_distr())
#' is_distr(create_one_div_x_distr())
#' is_distr(create_poisson_distr())
#' is_distr(create_uniform_distr())
#'
#' # FALSE
#' is_distr(NA)
#' is_distr(NULL)
#' is_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_distr <- function( # nolint simplification of this will hurt readablity
  x
) {
  if (beautier::is_beta_distr(x)) return(TRUE)
  if (beautier::is_exp_distr(x)) return(TRUE)
  if (beautier::is_gamma_distr(x)) return(TRUE)
  if (beautier::is_inv_gamma_distr(x)) return(TRUE)
  if (beautier::is_laplace_distr(x)) return(TRUE)
  if (beautier::is_log_normal_distr(x)) return(TRUE)
  if (beautier::is_normal_distr(x)) return(TRUE)
  if (beautier::is_one_div_x_distr(x)) return(TRUE)
  if (beautier::is_poisson_distr(x)) return(TRUE)
  if (beautier::is_uniform_distr(x)) return(TRUE)
  FALSE
}

#' Determine if the object is a valid
#' beta distribution,
#' as created by \code{\link{create_beta_distr}}
#' @param x an object, to be determined if it is a valid
#'   beta distribution,
#' @return TRUE if x is a valid beta distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_beta_distr(create_beta_distr())
#' # FALSE
#' is_beta_distr(create_exp_distr())
#' is_beta_distr(NA)
#' is_beta_distr(NULL)
#' is_beta_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_beta_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "beta") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$alpha)) return(FALSE)
  if (!"beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$beta)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' exponential distribution
#' as created by \code{\link{create_exp_distr}}
#' @param x an object, to be determined if it is a valid
#'   exponential distribution
#' @return TRUE if x is a valid exponential distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_exp_distr(create_exp_distr())
#' # FALSE
#' is_exp_distr(create_gamma_distr())
#' is_exp_distr(NA)
#' is_exp_distr(NULL)
#' is_exp_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_exp_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "exponential") return(FALSE)
  if (!"mean" %in% names(x)) return(FALSE)
  if (!beautier::is_mean_param(x$mean)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' gamma distribution,
#' as created by \code{\link{create_gamma_distr}}
#' @param x an object, to be determined if it is a valid
#'   gamma distribution
#' @return TRUE if x is a valid gamma distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_gamma_distr(create_gamma_distr())
#' # FALSE
#' is_gamma_distr(create_inv_gamma_distr())
#' is_gamma_distr(NA)
#' is_gamma_distr(NULL)
#' is_gamma_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_gamma_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "gamma") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$alpha)) return(FALSE)
  if (!"beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$beta)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' inverse-gamma distribution
#' as created by \code{\link{create_inv_gamma_distr}}
#' @param x an object, to be determined if it is a valid
#'   inverse-gamma distribution
#' @return TRUE if x is a valid inverse-gamma distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_inv_gamma_distr(create_inv_gamma_distr())
#' # FALSE
#' is_inv_gamma_distr(create_laplace_distr())
#' is_inv_gamma_distr(NA)
#' is_inv_gamma_distr(NULL)
#' is_inv_gamma_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_inv_gamma_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "inv_gamma") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!beautier::is_alpha_param(x$alpha)) return(FALSE)
  if (!"beta" %in% names(x)) return(FALSE)
  if (!beautier::is_beta_param(x$beta)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' Laplace distribution,
#' as created by \code{\link{create_laplace_distr}}
#' @param x an object, to be determined if it is a valid
#'   Laplace distribution
#' @return TRUE if x is a valid Laplace distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' # TRUE
#' is_laplace_distr(create_laplace_distr())
#' # FALSE
#' is_laplace_distr(create_log_normal_distr())
#' is_laplace_distr(NA)
#' is_laplace_distr(NULL)
#' is_laplace_distr("nonsense")
#' @export
is_laplace_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "laplace") return(FALSE)
  if (!"mu" %in% names(x)) return(FALSE)
  if (!beautier::is_mu_param(x$mu)) return(FALSE)
  if (!"scale" %in% names(x)) return(FALSE)
  if (!beautier::is_scale_param(x$scale)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' log-normal distribution,
#' as created by \code{\link{create_log_normal_distr}}
#' @param x an object, to be determined if it is a valid
#'   log-normal distribution
#' @return TRUE if x is a valid log-normal distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_log_normal_distr(create_log_normal_distr())
#' # FALSE
#' is_log_normal_distr(create_normal_distr())
#' is_distr(NA)
#' is_distr(NULL)
#' is_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_log_normal_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "log_normal") return(FALSE)
  if (!"m" %in% names(x)) return(FALSE)
  if (!beautier::is_m_param(x$m)) return(FALSE)
  if (!"s" %in% names(x)) return(FALSE)
  if (!beautier::is_s_param(x$s)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' normal distribution
#' as created by \code{\link{create_normal_distr}}
#' @param x an object, to be determined if it is a valid
#'   normal distribution
#' @return TRUE if x is a valid normal distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_normal_distr(create_normal_distr())
#' # FALSE
#' is_normal_distr(create_one_div_x_distr())
#' is_normal_distr(NA)
#' is_normal_distr(NULL)
#' is_normal_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_normal_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "normal") return(FALSE)
  if (!"mean" %in% names(x)) return(FALSE)
  if (!beautier::is_mean_param(x$mean)) return(FALSE)
  if (!"sigma" %in% names(x)) return(FALSE)
  if (!beautier::is_sigma_param(x$sigma)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' 1/x distribution,
#' as created by \code{\link{create_one_div_x_distr}}
#' @param x an object, to be determined if it is a valid
#'   1/x distribution
#' @return TRUE if x is a valid 1/x distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_one_div_x_distr(create_one_div_x_distr())
#' # FALSE
#' is_one_div_x_distr(create_poisson_distr())
#' is_one_div_x_distr(NA)
#' is_one_div_x_distr(NULL)
#' is_one_div_x_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_one_div_x_distr <- function(
  x
) {
  "name" %in% names(x) && x$name == "one_div_x"
}

#' Determine if the object is a valid
#' Poisson distribution
#' as created by \code{\link{create_poisson_distr}}
#' @param x an object, to be determined if it is a valid
#'   Poisson distribution
#' @return TRUE if x is a valid Poisson distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_poisson_distr(create_poisson_distr())
#' # FALSE
#' is_poisson_distr(create_uniform_distr())
#' is_distr(NA)
#' is_distr(NULL)
#' is_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_poisson_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "poisson") return(FALSE)
  if (!"lambda" %in% names(x)) return(FALSE)
  if (!beautier::is_lambda_param(x$lambda)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' uniform distribution
#' as created by \code{\link{create_uniform_distr}}
#' @param x an object, to be determined if it is a valid
#'   uniform distribution
#' @return TRUE if x is a valid uniform distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_uniform_distr(create_uniform_distr())
#' # FALSE
#' is_uniform_distr(create_beta_distr())
#' is_uniform_distr(NA)
#' is_uniform_distr(NULL)
#' is_uniform_distr("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_uniform_distr <- function(
  x
) {
  "name" %in% names(x) && x$name == "uniform"
}
