#' Create a site model
#' @param name the site model name. Valid
#'   names can be found in 'get_site_model_names'
#' @return a site_model
#' @export
create_site_model <- function(
  name
) {
  if (!is_site_model_name(name)) {
    site_models_as_string <- function() {
      s <- NULL
      for (p in get_site_model_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid site model name, must be one these: ",
      site_models_as_string()
    )
    stop("invalid site model name")
  }
  site_model <- list(name = name)
  site_model
}
