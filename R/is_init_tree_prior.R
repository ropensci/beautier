#' Determine if x is an initialized tree_prior objects
#' @param x the object to check if it is an
#'   initialized tree_priors object
#' @return TRUE if x is an initialized tree_prior object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_tree_prior <- function(
  x
) {
  if (!is_tree_prior(x)) return(FALSE)
  if (is_bd_tree_prior(x)) {
    return(is_init_bd_tree_prior(x))
  } else if (is_cbs_tree_prior(x)) {
    return(is_init_cbs_tree_prior(x))
  } else if (is_ccp_tree_prior(x)) {
    return(is_init_ccp_tree_prior(x))
  } else if (is_cep_tree_prior(x)) {
    return(is_init_cep_tree_prior(x))
  } else {
    check_true(is_yule_tree_prior(x))
    return(is_init_yule_tree_prior(x))
  }
}

#' Determine if x is an initialized Birth-Death tree_prior object
#' @param x the object to check if it is an
#'   initialized Birth-Death tree prior object
#' @return TRUE if x is an initialized Birth-Death tree_prior object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_bd_tree_prior <- function(
  x
) {
  check_true(is_bd_tree_prior(x))
  is_init_distr(x$birth_rate_distr) &&
    is_init_distr(x$death_rate_distr)
}

#' Determine if x is an initialized Coalescent Bayesian Skyline
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Bayesian Skyline tree prior object
#' @return TRUE if x is an initialized Coalescent Bayesian Skyline
#'   tree prior object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_cbs_tree_prior <- function(
  x
) {
  check_true(is_cbs_tree_prior(x))

  # Yup, is always initialized
  TRUE
}

#' Determine if x is an initialized Coalescent Constant Population
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Constant Population tree prior object
#' @return TRUE if x is an initialized Coalescent Constant Population
#'   tree prior object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_ccp_tree_prior <- function(
  x
) {
  is_init_distr(x$pop_size_distr)
}

#' Determine if x is an initialized Coalescent Exponential Population
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Exponential Population tree prior object
#' @return TRUE if x is an initialized Coalescent Exponential Population
#'   tree prior object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_cep_tree_prior <- function(
  x
) {
  check_true(is_cep_tree_prior(x))
  is_init_distr(x$pop_size_distr) &&
    is_init_distr(x$growth_rate_distr)
}

#' Determine if x is an initialized Yule tree_prior object
#' @param x the object to check if it is an
#'   initialized Yule tree prior object
#' @return TRUE if x is an initialized Yule tree_prior object
#' @author Richèl J.C. Bilderbeek
#' @export
is_init_yule_tree_prior <- function(
  x
) {
  check_true(is_yule_tree_prior(x))
  is_init_distr(x$birth_rate_distr)
}
