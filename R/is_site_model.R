#' Determine if the object is a valid site_model
#' @param x an object, to be determined if it is a site_model
#' @return TRUE if the site_model is a valid site_model, FALSE otherwise
#' @seealso  A site model can be created using \code{\link{create_site_model}}
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_site_model(create_gtr_site_model())
#' is_site_model(create_hky_site_model())
#' is_site_model(create_jc69_site_model())
#' is_site_model(create_tn93_site_model())
#'
#' # FALSE
#' is_site_model(NA)
#' is_site_model(NULL)
#' is_site_model("nonsense")
#' is_site_model(create_strict_clock_model())
#' is_site_model(create_bd_tree_prior())
#' is_site_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_site_model <- function(
  x
) {
  result <- FALSE
  tryCatch({
    check_site_model(x)
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}

#' Determine if the object is a valid GTR site model,
#' as created by \code{\link{create_gtr_site_model}}
#' @param x an object, to be determined if it is a valid GTR site model
#' @return TRUE if x is a valid GTR site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # site models
#' is_gtr_site_model(create_gtr_site_model())
#' is_gtr_site_model(create_hky_site_model())
#' is_gtr_site_model(create_jc69_site_model())
#' is_gtr_site_model(create_tn93_site_model())
#'
#' # other models
#' is_gtr_site_model(NA)
#' is_gtr_site_model(NULL)
#' is_gtr_site_model("nonsense")
#' is_gtr_site_model(create_strict_clock_model())
#' is_gtr_site_model(create_bd_tree_prior())
#' is_gtr_site_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_gtr_site_model <- function(
  x
) {
  result <- FALSE
  tryCatch({
    check_gtr_site_model(x)
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}

#' Determine if the object is a valid HKY site model,
#' as created by \code{\link{create_hky_site_model}}
#' @param x an object, to be determined if it is a valid HKY site model
#' @return TRUE if x is a valid HKY site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # site models
#' is_hky_site_model(create_hky_site_model())
#' is_hky_site_model(create_gtr_site_model())
#' is_hky_site_model(create_jc69_site_model())
#' is_hky_site_model(create_tn93_site_model())
#'
#' # other models
#' is_hky_site_model(NA)
#' is_hky_site_model(NULL)
#' is_hky_site_model("nonsense")
#' is_hky_site_model(create_strict_clock_model())
#' is_hky_site_model(create_bd_tree_prior())
#' is_hky_site_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_hky_site_model <- function( # nolint indeed a function with high cyclomatic complexity
  x
) {
  if (!is_site_model(x)) return(FALSE)
  if (x$name != "HKY") return(FALSE)
  if (!"kappa_param" %in% names(x)) return(FALSE)
  if (!"kappa_prior_distr" %in% names(x)) return(FALSE)
  if (!is_distr(x$kappa_prior_distr)) return(FALSE)
  if (!"freq_equilibrium" %in% names(x)) return(FALSE)
  if (!is_kappa_param(x$kappa_param)) return(FALSE)
  if (!is_freq_equilibrium_name(x$freq_equilibrium)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid JC69 site model
#' @param x an object, to be determined if it is a valid JC69 site model
#' @return TRUE if x is a valid JC69 site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # site models
#' is_jc69_site_model(create_gtr_site_model())
#' is_jc69_site_model(create_hky_site_model())
#' is_jc69_site_model(create_jc69_site_model())
#' is_jc69_site_model(create_tn93_site_model())
#'
#' # other models
#' is_jc69_site_model(NA)
#' is_jc69_site_model(NULL)
#' is_jc69_site_model("nonsense")
#' is_jc69_site_model(create_strict_clock_model())
#' is_jc69_site_model(create_bd_tree_prior())
#' is_jc69_site_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_jc69_site_model <- function(
  x
) {
  if (!is_site_model(x)) return(FALSE)
  if (x$name != "JC69") return(FALSE)
  TRUE
}

#' Determine if the object is a valid TN93 site model,
#' @param x an object, to be determined if it is a valid TN93 site model,
#'   as created by \code{\link{create_tn93_site_model}}
#' @return TRUE if x is a valid TN93 site model, FALSE otherwise
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # site models
#' is_tn93_site_model(create_gtr_site_model())
#' is_tn93_site_model(create_hky_site_model())
#' is_tn93_site_model(create_jc69_site_model())
#' is_tn93_site_model(create_tn93_site_model())
#'
#' # other models
#' is_tn93_site_model(NA)
#' is_tn93_site_model(NULL)
#' is_tn93_site_model("nonsense")
#' is_tn93_site_model("")
#' is_tn93_site_model(c())
#' is_tn93_site_model(create_strict_clock_model())
#' is_tn93_site_model(create_bd_tree_prior())
#' is_tn93_site_model(create_mcmc())
#'
#' check_empty_beautier_folder()
#' @export
is_tn93_site_model <- function(
  x
) {
  result <- FALSE
  tryCatch({
    check_tn93_site_model(x)
    result <- TRUE
  },
    error = function(e) {} # nolint do not care about e
  )
  result
}
