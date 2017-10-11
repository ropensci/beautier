#' Create a site model
#' @param name the site model name. Valid
#'   names can be found in 'get_site_model_names'
#' @return a site_model
#' @export
create_site_model <- function(
  name
) {
  if (!is_site_model_name(name)) {
    stop("invalid site model name")
  }
  site_model <- list(name = name)
  site_model
}
