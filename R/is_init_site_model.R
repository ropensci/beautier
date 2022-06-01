#' Determine if x is an initialized site model,
#' as created by \code{\link{create_site_model}}
#' @param x the object to check if it is an
#'   initialized site_models object
#' @return TRUE if x is an initialized site model
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_site_model <- function(
  x
) {
  if (!beautier::is_site_model(x)) return(FALSE)
  if (beautier::is_gtr_site_model(x)) {
    return(beautier::is_init_gtr_site_model(x))
  } else if (beautier::is_hky_site_model(x)) {
    return(beautier::is_init_hky_site_model(x))
  } else if (beautier::is_jc69_site_model(x)) {
    return(beautier::is_init_jc69_site_model(x))
  } else {
    testit::assert(beautier::is_tn93_site_model(x))
    return(beautier::is_init_tn93_site_model(x))
  }
}

#' Determine if x is an initialized GTR site model
#' as created by \code{\link{create_gtr_site_model}}
#' @param x the object to check if it is an
#'   initialized GTR site model
#' @return TRUE if x is an initialized GTR site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' gtr_site_model <- create_gtr_site_model()
#' # FALSE: not yet initialized
#' is_init_gtr_site_model(gtr_site_model)
#' gtr_site_model <- init_gtr_site_model(gtr_site_model)
#' # TRUE: now it is initialized
#' is_init_gtr_site_model(gtr_site_model)
#'
#' check_empty_beautier_folder()
#' @export
is_init_gtr_site_model <- function( # nolint simplification of this hurts readability
  x
) {
  if (!beautier::is_gtr_site_model(x)) return(FALSE)
  if (!beautier::is_init_distr(x$rate_ac_prior_distr)) return(FALSE)
  if (!beautier::is_init_distr(x$rate_ag_prior_distr)) return(FALSE)
  if (!beautier::is_init_distr(x$rate_at_prior_distr)) return(FALSE)
  if (!beautier::is_init_distr(x$rate_cg_prior_distr)) return(FALSE)
  # Indeed, no rate_ct_prior_distr yet
  if (!beautier::is_init_distr(x$rate_gt_prior_distr)) return(FALSE)
  if (!beautier::is_init_param(x$rate_ac_param)) return(FALSE)
  if (!beautier::is_init_param(x$rate_ag_param)) return(FALSE)
  if (!beautier::is_init_param(x$rate_at_param)) return(FALSE)
  if (!beautier::is_init_param(x$rate_cg_param)) return(FALSE)
  if (!beautier::is_init_param(x$rate_ct_param)) return(FALSE)
  if (!beautier::is_init_param(x$rate_gt_param)) return(FALSE)
  if (!beautier::is_init_gamma_site_model(x$gamma_site_model)) return(FALSE)
  TRUE
}

#' Determine if x is an initialized HKY site model
#' as created by \code{\link{create_hky_site_model}}
#' @param x the object to check if it is an
#'   initialized HKY site model
#' @return TRUE if x is an initialized HKY site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' hky_site_model <- create_hky_site_model()
#' # FALSE: not yet initialized
#' is_init_hky_site_model(hky_site_model)
#' hky_site_model <- init_hky_site_model(hky_site_model)
#' # TRUE: now it is initialized
#' is_init_hky_site_model(hky_site_model)
#'
#' check_empty_beautier_folder()
#' @export
is_init_hky_site_model <- function(
  x
) {
  testit::assert(beautier::is_hky_site_model(x))
  if (!beautier::is_init_gamma_site_model(x$gamma_site_model)) return(FALSE)
  beautier::is_init_distr(x$kappa_prior)
}

#' Determine if x is an initialized JC69 site model
#' as created by \code{\link{create_jc69_site_model}}
#' @param x the object to check if it is an
#'   initialized JC69 site model
#' @return TRUE if x is an initialized JC69 site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' jc69_site_model <- create_jc69_site_model(
#'   gamma_site_model = create_gamma_site_model(
#'     gamma_cat_count = 2,
#'     gamma_shape_prior_distr = create_normal_distr()
#'   )
#' )
#' # FALSE: not yet initialized
#' is_init_jc69_site_model(jc69_site_model)
#' jc69_site_model <- init_jc69_site_model(jc69_site_model)
#' # TRUE: now it is initialized
#' is_init_jc69_site_model(jc69_site_model)
#'
#' check_empty_beautier_folder()
#' @export
is_init_jc69_site_model <- function(
  x
) {
  testit::assert(beautier::is_jc69_site_model(x))
  if (!beautier::is_init_gamma_site_model(x$gamma_site_model)) return(FALSE)
  TRUE
}

#' Determine if x is an initialized tn93 site model
#' as created by \code{\link{create_tn93_site_model}}
#' @param x the object to check if it is an
#'   initialized TN93 site model
#' @return TRUE if x is an initialized TN93 site model
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' tn93_site_model <- create_tn93_site_model()
#' # FALSE: not yet initialized
#' is_init_tn93_site_model(tn93_site_model)
#' tn93_site_model <- init_tn93_site_model(tn93_site_model)
#' # TRUE: now it is initialized
#' is_init_tn93_site_model(tn93_site_model)
#'
#' check_empty_beautier_folder()
#' @export
is_init_tn93_site_model <- function(
  x
) {
  testit::assert(beautier::is_tn93_site_model(x))
  if (!beautier::is_init_gamma_site_model(x$gamma_site_model)) return(FALSE)
  beautier::is_init_distr(x$kappa_1_prior) &&
    beautier::is_init_distr(x$kappa_2_prior)
}
