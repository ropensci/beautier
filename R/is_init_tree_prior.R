#' Determine if x is an initialized tree_prior objects
#' @param x the object to check if it is an
#'   initialized tree_priors object
#' @return TRUE if x is an initialized tree_prior object
#' @author Richel J.C. Bilderbeek
is_init_tree_prior <- function(
  x
) {
  if (!beautier::is_tree_prior(x)) return(FALSE)
  if (beautier::is_bd_tree_prior(x)) {
    return(is_init_bd_tree_prior(x))  # nolint internal function call
  } else if (beautier::is_cbs_tree_prior(x)) {
    return(is_init_cbs_tree_prior(x))  # nolint internal function call
  } else if (beautier::is_ccp_tree_prior(x)) {
    return(is_init_ccp_tree_prior(x))  # nolint internal function call
  } else if (beautier::is_cep_tree_prior(x)) {
    return(is_init_cep_tree_prior(x))  # nolint internal function call
  } else {
    testit::assert(beautier::is_yule_tree_prior(x))
    return(is_init_yule_tree_prior(x))  # nolint internal function call
  }
}

#' Determine if x is an initialized Birth-Death tree_prior object
#' @param x the object to check if it is an
#'   initialized Birth-Death tree prior object
#' @return TRUE if x is an initialized Birth-Death tree_prior object
#' @author Richel J.C. Bilderbeek
is_init_bd_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_bd_tree_prior(x))
  return(is_init_distr(get_bd_birth_rate_distr(x)) &&
    is_init_distr(get_bd_death_rate_distr(x))
  )
}

#' Determine if x is an initialized Coalescent Bayesian Skyline
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Bayesian Skyline tree prior object
#' @return TRUE if x is an initialized Coalescent Bayesian Skyline
#'   tree prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_init_cbs_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_cbs_tree_prior(x))

  # Yup, is always initialized
  TRUE
}

#' Determine if x is an initialized Coalescent Constant Population
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Constant Population tree prior object
#' @return TRUE if x is an initialized Coalescent Constant Population
#'   tree prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_init_ccp_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_ccp_tree_prior(x))
  is_init_distr(get_ccp_pop_size_distr(x))
}

#' Determine if x is an initialized Coalescent Exponential Population
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Exponential Population tree prior object
#' @return TRUE if x is an initialized Coalescent Exponential Population
#'   tree prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_init_cep_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_cep_tree_prior(x))

  return(
    is_init_distr(get_cep_pop_size_distr(x)) &&
    is_init_distr(get_cep_growth_rate_distr(x))
  )
}

#' Determine if x is an initialized Yule tree_prior object
#' @param x the object to check if it is an
#'   initialized Yule tree prior object
#' @return TRUE if x is an initialized Yule tree_prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_init_yule_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_yule_tree_prior(x))
  is_init_distr(get_yule_birth_rate_distr(x))
}
