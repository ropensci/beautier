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
#' @noRd
is_distr <- function(
  x
) {
  if (is_beta_distr(x)) return(TRUE) # nolint beautier function
  if (is_exp_distr(x)) return(TRUE) # nolint beautier function
  if (is_gamma_distr(x)) return(TRUE) # nolint beautier function
  if (is_inv_gamma_distr(x)) return(TRUE) # nolint beautier function
  if (is_laplace_distr(x)) return(TRUE) # nolint beautier function
  if (is_log_normal_distr(x)) return(TRUE) # nolint beautier function
  if (is_normal_distr(x)) return(TRUE) # nolint beautier function
  if (is_one_div_x_distr(x)) return(TRUE) # nolint beautier function
  if (is_poisson_distr(x)) return(TRUE) # nolint beautier function
  if (is_uniform_distr(x)) return(TRUE) # nolint beautier function
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
#' @noRd
is_beta_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "beta") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!is_alpha_param(x$alpha)) return(FALSE) # nolint beautier function
  if (!"beta" %in% names(x)) return(FALSE)
  if (!is_beta_param(x$beta)) return(FALSE) # nolint beautier function
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
#' @noRd
is_exp_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "exponential") return(FALSE)
  if (!"mean" %in% names(x)) return(FALSE)
  if (!is_mean_param(x$mean)) return(FALSE) # nolint beautier function
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
#' @noRd
is_gamma_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "gamma") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!is_alpha_param(x$alpha)) return(FALSE) # nolint beautier function
  if (!"beta" %in% names(x)) return(FALSE)
  if (!is_beta_param(x$beta)) return(FALSE) # nolint beautier function
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
#' @noRd
is_inv_gamma_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "inv_gamma") return(FALSE)
  if (!"alpha" %in% names(x)) return(FALSE)
  if (!is_alpha_param(x$alpha)) return(FALSE) # nolint beautier function
  if (!"beta" %in% names(x)) return(FALSE)
  if (!is_beta_param(x$beta)) return(FALSE) # nolint beautier function
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
#'   laplace_distr <- create_laplace_distr()
#'   testit::assert(beautier:::is_laplace_distr(laplace_distr))
#' @noRd
is_laplace_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "laplace") return(FALSE)
  if (!"mu" %in% names(x)) return(FALSE)
  if (!is_mu_param(x$mu)) return(FALSE) # nolint beautier function
  if (!"scale" %in% names(x)) return(FALSE)
  if (!is_scale_param(x$scale)) return(FALSE) # nolint beautier function
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
#'   log_normal_distr <- create_log_normal_distr()
#'
#'   input_fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")
#'   beast2_input_file <- tempfile(fileext = ".xml")
#'   create_beast2_input_file(
#'     input_filename = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_prior = create_yule_tree_prior(
#'       birth_rate_distr = log_normal_distr
#'     )
#'   )
#'   testit::assert(file.exists(beast2_input_file))
#' @noRd
is_log_normal_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "log_normal") return(FALSE)
  if (!"m" %in% names(x)) return(FALSE)
  if (!is_m_param(x$m)) return(FALSE) # nolint beautier function
  if (!"s" %in% names(x)) return(FALSE)
  if (!is_s_param(x$s)) return(FALSE) # nolint beautier function
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
#' @noRd
is_normal_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "normal") return(FALSE)
  if (!"mean" %in% names(x)) return(FALSE)
  if (!is_mean_param(x$mean)) return(FALSE) # nolint beautier function
  if (!"sigma" %in% names(x)) return(FALSE)
  if (!is_sigma_param(x$sigma)) return(FALSE) # nolint beautier function
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
#' @noRd
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
#' @noRd
is_poisson_distr <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "poisson") return(FALSE)
  if (!"lambda" %in% names(x)) return(FALSE)
  if (!is_lambda_param(x$lambda)) return(FALSE) # nolint beautier function
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
#' @noRd
is_uniform_distr <- function(
  x
) {
  "name" %in% names(x) && x$name == "uniform"
}
