#' Creates all supported site models
#'   which is just a list of the types returned by
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   \code{\link{create_jc69_site_model}}
#'   and \code{\link{create_tn93_site_model}}
#' @return a list of site_models
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # All created site models are a kind of site model
#'  site_models <- beautier:::create_site_models()
#'  testit::assert(beautier:::is_gtr_site_model(site_models[[1]]))
#'  testit::assert(beautier:::is_hky_site_model(site_models[[2]]))
#'  testit::assert(beautier:::is_jc69_site_model(site_models[[3]]))
#'  testit::assert(beautier:::is_tn93_site_model(site_models[[4]]))
#'
#'  # Names are conformant
#'  for (site_model in site_models) {
#'    testit::assert(site_model$name %in% get_site_model_names())
#'  }
create_site_models <- function() {
  return(
    list(
      beautier::create_gtr_site_model(),
      beautier::create_hky_site_model(),
      beautier::create_jc69_site_model(),
      beautier::create_tn93_site_model()
    )
  )
}
