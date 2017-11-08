#' Determine if x is an initialized tree_prior objects
#' @param x the object to check if it is an
#'   initialized tree_priors object
#' @return TRUE if x is an initialized tree_prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_initialized_tree_prior <- function(
  x
) {
  if (!is_tree_prior(x)) return(FALSE)
  if (is_yule_tree_prior(x)) {
    return(is_initialized_yule_tree_prior(x))
  }
  return(TRUE)
}

#' Determine if x is an initialized Yule tree_prior object
#' @param x the object to check if it is an
#'   initialized Yule tree prior object
#' @return TRUE if x is an initialized tree_prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_initialized_yule_tree_prior <- function(
  x
) {
  if (!is_yule_tree_prior(x)) {
    stop("Must supply a Yule tree prior object")
  }
  return(
    !is.na(
      get_distribution_id(
        get_yule_birth_rate_distr(x)
      )
    )
  )
}
