#' Determine if an object is a valid tree prior
#' @param x an object
#' @return TRUE if x is a valid tree_prior, FALSE otherwise
#' @seealso tree priors can be created by \code{\link{create_tree_prior}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' is_tree_prior(create_bd_tree_prior())
#' is_tree_prior(create_yule_tree_prior())
#' !is_tree_prior("nonsense")
#'
#' check_empty_beautier_folder()
#' @export
is_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (!is_tree_prior_name(x$name)) return(FALSE)
  if (!"id" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid Birth Death tree prior
#' @param x an object, to be determined if it is a valid birth death tree prior
#' @return TRUE if x is a valid birth death tree prior, FALSE otherwise
#' @seealso Use \code{\link{create_bd_tree_prior}} to create a valid
#'   Birth-Death tree prior
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#'   is_bd_tree_prior(create_bd_tree_prior())
#'   !is_bd_tree_prior(create_cbs_tree_prior())
#'   !is_bd_tree_prior(create_ccp_tree_prior())
#'   !is_bd_tree_prior(create_cep_tree_prior())
#'   !is_bd_tree_prior(create_yule_tree_prior())
#'
#' check_empty_beautier_folder()
#' @export
is_bd_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "birth_death") return(FALSE)
  if (!"birth_rate_distr" %in% names(x)) return(FALSE)
  if (!"death_rate_distr" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid constant coalescent Bayesian skyline prior
#' @param x an object, to be determined if it is a valid constant coalescent
#'   Bayesian skyline prior
#' @return `TRUE` if `x` is a valid constant coalescent Bayesian skyline prior,
#'   `FALSE` otherwise
#' @seealso Use \code{\link{create_cbs_tree_prior}} to create a valid
#'   coalescent Bayes skyline tree prior
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_cbs_tree_prior(create_cbs_tree_prior())
#'
#' # FALSE
#' is_cbs_tree_prior(create_bd_tree_prior())
#' is_cbs_tree_prior(create_ccp_tree_prior())
#' is_cbs_tree_prior(create_cep_tree_prior())
#' is_cbs_tree_prior(create_yule_tree_prior())
#'
#' check_empty_beautier_folder()
#' @export
is_cbs_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "coalescent_bayesian_skyline") return(FALSE)
  if (!"group_sizes_dimension" %in% names(x)) return(FALSE)
  if (!"b_pop_sizes_param" %in% names(x)) return(FALSE)
  if (!"pop_sizes_scaler_scale_factor" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#'   constant coalescence population tree prior
#' @param x an object, to be determined if it is a valid
#'   constant coalescence population tree prior
#' @return TRUE if x is a valid constant coalescence population tree prior,
#'   FALSE otherwise
#' @seealso Use \code{\link{create_ccp_tree_prior}} to create a valid
#'   constant coalescence population tree prior
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'   !is_ccp_tree_prior(create_bd_tree_prior())
#'   !is_ccp_tree_prior(create_cbs_tree_prior())
#'   is_ccp_tree_prior(create_ccp_tree_prior())
#'   !is_ccp_tree_prior(create_cep_tree_prior())
#'   !is_ccp_tree_prior(create_yule_tree_prior())
#' check_empty_beautier_folder()
#' @export
is_ccp_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "coalescent_constant_population") return(FALSE)
  if (!"pop_size_distr" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid
#' coalescent exponential population tree prior
#' @param x an object, to be determined if it is a valid
#'   constant coalescent exponential population tree prior
#' @return TRUE if x is a valid coalescent exponential population tree prior,
#'   FALSE otherwise
#' @seealso Use \code{\link{create_cep_tree_prior}} to create a valid
#'   coalescent exponential population tree prior
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#'   !is_cep_tree_prior(create_bd_tree_prior())
#'   !is_cep_tree_prior(create_cbs_tree_prior())
#'   !is_cep_tree_prior(create_ccp_tree_prior())
#'   is_cep_tree_prior(create_cep_tree_prior())
#'   !is_cep_tree_prior(create_yule_tree_prior())
#'
#' check_empty_beautier_folder()
#' @export
is_cep_tree_prior <- function(
  x
) {
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "coalescent_exp_population") return(FALSE)
  if (!"pop_size_distr" %in% names(x)) return(FALSE)
  if (!"growth_rate_distr" %in% names(x)) return(FALSE)
  TRUE
}

#' Determine if the object is a valid Yule tree prior,
#' @param x an object, to be determined if it is a valid Yule tree prior
#' @return TRUE if x is a valid Yule tree prior, FALSE otherwise
#' @seealso Use \code{\link{create_yule_tree_prior}} to create a valid
#'   Yule tree prior
#' @author Richèl J.C. Bilderbeek
#' @examples
#' check_empty_beautier_folder()
#'
#' # TRUE
#' is_yule_tree_prior(create_yule_tree_prior())
#'
#' # FALSE
#' is_yule_tree_prior(create_bd_tree_prior())
#' is_yule_tree_prior(create_cbs_tree_prior())
#' is_yule_tree_prior(create_ccp_tree_prior())
#' is_yule_tree_prior(create_cep_tree_prior())
#'
#' check_empty_beautier_folder()
#' @export
is_yule_tree_prior <- function(
  x
) {
  if (is.list(x) && length(x) == 1) return(is_yule_tree_prior(x[[1]]))
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "yule") return(FALSE)
  if (!"birth_rate_distr" %in% names(x)) return(FALSE)
  TRUE
}
