#' Creates all supported site models
#' @return a list of site_models
#' @export
create_site_models <- function() {
  return(
    list(
      create_site_model(name = "JC69"),
      create_site_model(name = "HKY"),
      create_site_model(name = "TN93"),
      create_site_model(name = "GTR")
    )
  )
}
