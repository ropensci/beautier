#' Creates all supported site models
#' @return a list of site_models
#' @export
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
