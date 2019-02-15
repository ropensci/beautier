#' Determine if x is an initialized tree_prior objects
#' @param x the object to check if it is an
#'   initialized tree_priors object
#' @return TRUE if x is an initialized tree_prior object
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_tree_prior <- function(
  x
) {
  if (!is_tree_prior(x)) return(FALSE) # nolint beautier function
  if (is_bd_tree_prior(x)) { # nolint beautier function
    return(is_init_bd_tree_prior(x))  # nolint beautier function call
  } else if (is_cbs_tree_prior(x)) { # nolint beautier function
    return(is_init_cbs_tree_prior(x))  # nolint beautier function call
  } else if (is_ccp_tree_prior(x)) { # nolint beautier function
    return(is_init_ccp_tree_prior(x))  # nolint beautier function call
  } else if (is_cep_tree_prior(x)) { # nolint beautier function
    return(is_init_cep_tree_prior(x))  # nolint beautier function call
  } else {
    testit::assert(is_yule_tree_prior(x)) # nolint beautier function
    return(is_init_yule_tree_prior(x))  # nolint beautier function call
  }
}

#' Determine if x is an initialized Birth-Death tree_prior object
#' @param x the object to check if it is an
#'   initialized Birth-Death tree prior object
#' @return TRUE if x is an initialized Birth-Death tree_prior object
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_bd_tree_prior <- function(
  x
) {
  testit::assert(is_bd_tree_prior(x)) # nolint beautier function
  is_init_distr(x$birth_rate_distr) && # nolint beautier function
    is_init_distr(x$death_rate_distr) # nolint beautier function
}

#' Determine if x is an initialized Coalescent Bayesian Skyline
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Bayesian Skyline tree prior object
#' @return TRUE if x is an initialized Coalescent Bayesian Skyline
#'   tree prior object
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_cbs_tree_prior <- function(
  x
) {
  testit::assert(is_cbs_tree_prior(x)) # nolint beautier function

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
#' @noRd
is_init_ccp_tree_prior <- function(
  x
) {
  is_init_distr(x$pop_size_distr) # nolint beautier function
}

#' Determine if x is an initialized Coalescent Exponential Population
#'   tree_prior object
#' @param x the object to check if it is an
#'   initialized Coalescent Exponential Population tree prior object
#' @return TRUE if x is an initialized Coalescent Exponential Population
#'   tree prior object
#' @author Richèl J.C. Bilderbeek
is_init_cep_tree_prior <- function(
  x
) {
  testit::assert(is_cep_tree_prior(x)) # nolint beautier function
  is_init_distr(x$pop_size_distr) &&  # nolint beautier function
  is_init_distr(x$growth_rate_distr)  # nolint beautier function
}

#' Determine if x is an initialized Yule tree_prior object
#' @param x the object to check if it is an
#'   initialized Yule tree prior object
#' @return TRUE if x is an initialized Yule tree_prior object
#' @author Richèl J.C. Bilderbeek
#' @noRd
is_init_yule_tree_prior <- function(
  x
) {
  testit::assert(is_yule_tree_prior(x)) # nolint beautier function
  is_init_distr(x$birth_rate_distr) # nolint beautier function
}
