#' Determine if the object is a valid site_model
#' @param x an object, to be determined if it is a site_model
#' @return TRUE if the site_model is a valid site_model, FALSE otherwise
#' @seealso  A site model can be created using \code{\link{create_site_model}}
#' @examples
#'   # site models
#'   testit::assert(beautier:::is_site_model(create_gtr_site_model()))
#'   testit::assert(beautier:::is_site_model(create_hky_site_model()))
#'   testit::assert(beautier:::is_site_model(create_jc69_site_model()))
#'   testit::assert(beautier:::is_site_model(create_tn93_site_model()))
#'
#'   # other models
#'   testit::assert(!beautier:::is_site_model(create_strict_clock_model()))
#'   testit::assert(!beautier:::is_site_model(create_bd_tree_prior()))
#'   testit::assert(!beautier:::is_site_model(create_mcmc()))
is_site_model <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!is_site_model_name(x$name)) return(FALSE)
  if (!"id" %in% names(x)) return(FALSE)
  if (!"gamma_site_model" %in% names(x)) return(FALSE)
  if (!is_gamma_site_model(x$gamma_site_model)) return(FALSE)
  TRUE
}
