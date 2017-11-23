#' Get the number of distributions a site model has
#' @param site_model a site_model,
#'   as created by \code{\link{create_site_model}}
#' @return the number of distributions a site model has
#' @author Richel J.C. Bilderbeek
#' @export
get_site_model_n_distrs <- function(
  site_model
) {
  if (!is_site_model(site_model)) {
    stop("'site_model' must be a site model")
  }
  if (is_gtr_site_model(site_model)) {
    return(0)
  } else if (is_hky_site_model(site_model)) {
    return(1)
  } else if (is_jc69_site_model(site_model)) {
    return(0)
  } else {
    testit::assert(is_tn93_site_model(site_model)) # nolint internal function
    return(2)
  }
}
