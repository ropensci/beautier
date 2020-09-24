#' Creates all supported site models
#'   which is a list of the types returned by
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   \code{\link{create_jc69_site_model}}
#'   and \code{\link{create_tn93_site_model}}
#' @return a list of site_models
#' @seealso Use \link{create_site_model} to create a site model
#' @examples
#' # All created site models are a kind of site model
#' site_models <- create_site_models()
#'
#' # TRUE
#' is_gtr_site_model(site_models[[1]])
#' is_hky_site_model(site_models[[2]])
#' is_jc69_site_model(site_models[[3]])
#' is_tn93_site_model(site_models[[4]])
#' @author Richèl J.C. Bilderbeek
#' @export
create_site_models <- function() {
  list(
    beautier::create_gtr_site_model(),
    beautier::create_hky_site_model(),
    beautier::create_jc69_site_model(),
    beautier::create_tn93_site_model()
  )
}
