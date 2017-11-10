#' Determine if x is an initialized tree_prior objects
#' @param x the object to check if it is an
#'   initialized tree_priors object
#' @return TRUE if x is an initialized tree_prior object
#' @author Richel J.C. Bilderbeek
is_initialized_tree_prior <- function(
  x
) {
  if (!beautier::is_tree_prior(x)) return(FALSE)
  if (beautier::is_bd_tree_prior(x)) {
    return(is_initialized_bd_tree_prior(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_cbs_tree_prior(x)) {
    return(is_initialized_cbs_tree_prior(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_ccp_tree_prior(x)) {
    return(is_initialized_ccp_tree_prior(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else if (beautier::is_cep_tree_prior(x)) {
    return(is_initialized_cep_tree_prior(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  } else {
    testit::assert(beautier::is_yule_tree_prior(x))
    return(is_initialized_yule_tree_prior(x))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  }
}

#' Determine if x is an initialized Birth-Death tree_prior object
#' @param x the object to check if it is an
#'   initialized Birth-Death tree prior object
#' @return TRUE if x is an initialized Birth-Death tree_prior object
#' @author Richel J.C. Bilderbeek
is_initialized_bd_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_bd_tree_prior(x))
  return(is_initialized_distr(get_bd_birth_rate_distr(x)) &&
    is_initialized_distr(get_bd_death_rate_distr(x))
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
is_initialized_cbs_tree_prior <- function(
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
is_initialized_ccp_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_ccp_tree_prior(x))
  is_initialized_distr(get_ccp_pop_size_distr(x))
}

#' Determine if x is an initialized Coalescent Exponential Population
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Exponential Population tree prior object
#' @return TRUE if x is an initialized Coalescent Exponential Population
#'   tree prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_initialized_cep_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_cep_tree_prior(x))

  return(
    is_initialized_distr(get_cep_pop_size_distr(x)) &&
    is_initialized_distr(get_cep_growth_rate_distr(x))
  )
}

#' Determine if x is an initialized Yule tree_prior object
#' @param x the object to check if it is an
#'   initialized Yule tree prior object
#' @return TRUE if x is an initialized Yule tree_prior object
#' @author Richel J.C. Bilderbeek
#' @export
is_initialized_yule_tree_prior <- function(
  x
) {
  testit::assert(beautier::is_yule_tree_prior(x))
  is_initialized_distr(get_yule_birth_rate_distr(x))
}
