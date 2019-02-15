#' Determine if x is an initialized site model,
#' as created by \code{\link{create_site_model}}
#' @param x the object to check if it is an
#'   initialized site_models object
#' @return TRUE if x is an initialized site model
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE) # nolint beautier function
  if (is_gtr_site_model(x)) { # nolint beautier function
    return(is_init_gtr_site_model(x)) # nolint beautier function call
  } else if (is_hky_site_model(x)) { # nolint beautier function
    return(is_init_hky_site_model(x)) # nolint beautier function call
  } else if (is_jc69_site_model(x)) { # nolint beautier function
    return(is_init_jc69_site_model(x)) # nolint beautier function call
  } else {
    testit::assert(is_tn93_site_model(x)) # nolint beautier function
    return(is_init_tn93_site_model(x)) # nolint beautier function call
  }
}

#' Determine if x is an initialized GTR site model
#' as created by \code{\link{create_gtr_site_model}}
#' @param x the object to check if it is an
#'   initialized GTR site model
#' @return TRUE if x is an initialized GTR site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   gtr_site_model <- create_gtr_site_model()
#'   testit::assert(!beautier:::is_init_gtr_site_model(gtr_site_model))
#'   gtr_site_model <- beautier:::init_gtr_site_model(gtr_site_model)
#'   testit::assert(beautier:::is_init_gtr_site_model(gtr_site_model))
#' @noRd
is_init_gtr_site_model <- function(
  x
) {
  if (!is_gtr_site_model(x)) return(FALSE) # nolint beautier function
  if (!is_init_distr(x$rate_ac_prior_distr)) return(FALSE) # nolint beautier function
  if (!is_init_distr(x$rate_ag_prior_distr)) return(FALSE) # nolint beautier function
  if (!is_init_distr(x$rate_at_prior_distr)) return(FALSE) # nolint beautier function
  if (!is_init_distr(x$rate_cg_prior_distr)) return(FALSE) # nolint beautier function
  # Indeed, no rate_ct_prior_distr yet
  if (!is_init_distr(x$rate_gt_prior_distr)) return(FALSE) # nolint beautier function
  if (!is_init_param(x$rate_ac_param)) return(FALSE) # nolint beautier function
  if (!is_init_param(x$rate_ag_param)) return(FALSE) # nolint beautier function
  if (!is_init_param(x$rate_at_param)) return(FALSE) # nolint beautier function
  if (!is_init_param(x$rate_cg_param)) return(FALSE) # nolint beautier function
  if (!is_init_param(x$rate_ct_param)) return(FALSE) # nolint beautier function
  if (!is_init_param(x$rate_gt_param)) return(FALSE) # nolint beautier function
  if (!is_init_gamma_site_model(x$gamma_site_model)) return(FALSE) # nolint beautier function
  TRUE
}

#' Determine if x is an initialized hky site model
#' as created by \code{\link{create_hky_site_model}}
#' @param x the object to check if it is an
#'   initialized HKY site model
#' @return TRUE if x is an initialized HKY site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   hky_site_model <- create_hky_site_model()
#'   testit::assert(!beautier:::is_init_hky_site_model(hky_site_model))
#'   hky_site_model <- beautier:::init_hky_site_model(hky_site_model)
#'   testit::assert(beautier:::is_init_hky_site_model(hky_site_model))
#' @noRd
is_init_hky_site_model <- function(
  x
) {
  testit::assert(is_hky_site_model(x)) # nolint beautier function
  if (!is_init_gamma_site_model(x$gamma_site_model)) return(FALSE) # nolint beautier function
  is_init_distr(x$kappa_prior) # nolint beautier function
}

#' Determine if x is an initialized JC69 site model
#' as created by \code{\link{create_jc69_site_model}}
#' @param x the object to check if it is an
#'   initialized JC69 site model
#' @return TRUE if x is an initialized JC69 site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   jc69_site_model <- create_jc69_site_model()
#'   testit::assert(!beautier:::is_init_jc69_site_model(jc69_site_model))
#'   jc69_site_model <- beautier:::init_jc69_site_model(jc69_site_model)
#'   testit::assert(beautier:::is_init_jc69_site_model(jc69_site_model))
#' @noRd
is_init_jc69_site_model <- function(
  x
) {
  testit::assert(is_jc69_site_model(x)) # nolint beautier function
  if (!is_init_gamma_site_model(x$gamma_site_model)) return(FALSE) # nolint beautier function
  TRUE
}

#' Determine if x is an initialized tn93 site model
#' as created by \code{\link{create_tn93_site_model}}
#' @param x the object to check if it is an
#'   initialized TN93 site model
#' @return TRUE if x is an initialized TN93 site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   tn93_site_model <- create_tn93_site_model()
#'   testit::assert(!beautier:::is_init_tn93_site_model(tn93_site_model))
#'   tn93_site_model <- beautier:::init_tn93_site_model(tn93_site_model)
#'   testit::assert(beautier:::is_init_tn93_site_model(tn93_site_model))
#' @noRd
is_init_tn93_site_model <- function(
  x
) {
  testit::assert(is_tn93_site_model(x)) # nolint beautier function
  if (!is_init_gamma_site_model(x$gamma_site_model)) return(FALSE) # nolint beautier function
  is_init_distr(x$kappa_1_prior) && # nolint beautier function
    is_init_distr(x$kappa_2_prior) # nolint beautier function
}
