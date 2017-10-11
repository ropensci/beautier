#' Create a site model
#' @param name the site model name. Can be 'JC69', 'HKY', 'TN93' or 'GTR'
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
