#' General function to create a site model. Prefer using
#' 'create_jk69_site_model', 'create_hky_site_model',
#' 'create_tn93_site_model' and 'create_gtr_site_model',
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

#' Create a JC69 site model
#' @return a JC69 site_model
#' @export
create_jc69_site_model <- function() {
  return(create_site_model(name = "JC69"))
}

#' Create an HKY site model
#' @return an HKY site_model
#' @export
create_hky_site_model <- function() {
  return(create_site_model(name = "HKY"))
}

#' Create a TN93 site model
#' @return a TN93 site_model
#' @export
create_tn93_site_model <- function() {
  return(create_site_model(name = "TN93"))
}

#' Create a GTR site model
#' @return a GTR site_model
#' @export
create_gtr_site_model <- function() {
  return(create_site_model(name = "GTR"))
}
