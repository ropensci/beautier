#' Creates all supported site models
#' @return a list of site_models
#' @export
create_site_models <- function() {
  return(
    list(
      beastscriptr::create_gtr_site_model(),
      beastscriptr::create_hky_site_model(),
      beastscriptr::create_jc69_site_model(),
      beastscriptr::create_tn93_site_model()
    )
  )
}
