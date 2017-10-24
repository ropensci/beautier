#' General function to create a site model. Prefer using
#' 'create_jk69_site_model', 'create_hky_site_model',
#' 'create_tn93_site_model' and 'create_gtr_site_model',
#' @param name the site model name. Valid
#'   names can be found in 'get_site_model_names'
#' @param ... specific site model parameters
#' @return a site_model
#' @export
create_site_model <- function(
  name,
  ...
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
  site_model <- list(name = name, ...)
  site_model
}

#' Create a JC69 site model
#' @return a JC69 site_model
#' @export
create_jc69_site_model <- function() {
  return(beastscriptr::create_site_model(name = "JC69"))
}

#' Create an HKY site model
#' @param gamma_cat_count the number of gamma categories, must
#'   be an integer with value zero or more
#' @param kappa the kappa
#' @param prop_invariant the proportion invariant, must be a value
#'   from 0.0 to 1.0
#' @return an HKY site_model
#' @export
create_hky_site_model <- function(
  gamma_cat_count = get_default_gamma_cat_count(),
  kappa = get_default_kappa(),
  prop_invariant = get_default_prop_invariant()
) {
  return(
    beastscriptr::create_site_model(
      name = "HKY",
      gamma_cat_count = gamma_cat_count,
      kappa = kappa,
      prop_invariant = prop_invariant
    )
  )
}

#' Create a TN93 site model
#' @return a TN93 site_model
#' @export
create_tn93_site_model <- function() {
  return(beastscriptr::create_site_model(name = "TN93"))
}

#' Create a GTR site model
#' @return a GTR site_model
#' @export
create_gtr_site_model <- function() {
  return(beastscriptr::create_site_model(name = "GTR"))
}
