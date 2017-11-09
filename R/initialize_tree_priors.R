#' Initializes all tree priors
#' @param tree_priors a list of one or more tree priors to be initialized.
#'   Tree priors can be created using \code{\link{create_tree_prior}}
#' @param distr_id the first distributions' ID
#' @return a list of initialized tree priors
#' @author Richel J.C. Bilderbeek
initialize_tree_priors <- function(
  tree_priors,
  distr_id = 0
) {
  testit::assert(beautier::are_tree_priors(tree_priors))

  id <- distr_id # Notational convenience

  for (i in seq_along(tree_priors)) {
    tree_prior <- tree_priors[[i]]
    testit::assert(beautier::is_tree_prior(tree_prior))

    if (is_bd_tree_prior(tree_prior)) {

      if (!is_initialized_bd_tree_prior(tree_prior)) {

        tree_prior <- initialize_bd_tree_prior(tree_prior, id = id)  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        testit::assert(is_initialized_bd_tree_prior(tree_prior))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        id <- id + 2 # Has two distributions
      }
    } else if (is_ccp_tree_prior(tree_prior)) {

      if (!is_initialized_ccp_tree_prior(tree_prior)) {

        tree_prior <- initialize_ccp_tree_prior(tree_prior, id = id)  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        testit::assert(is_initialized_ccp_tree_prior(tree_prior))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        id <- id + 1 # Has one distribution
      }
    } else if (is_cep_tree_prior(tree_prior)) {
      if (!is_initialized_cep_tree_prior(tree_prior)) {

        tree_prior <- initialize_cep_tree_prior(tree_prior, id = id)  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        testit::assert(is_initialized_cep_tree_prior(tree_prior))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        id <- id + 2 # Has two distribution
      }
    } else if (is_yule_tree_prior(tree_prior)) {

      if (!is_initialized_yule_tree_prior(tree_prior)) {

        tree_prior <- initialize_yule_tree_prior(tree_prior, id = id)  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        testit::assert(is_initialized_yule_tree_prior(tree_prior))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        id <- id + 1 # Has one distribution
      }
    }

    tree_priors[[i]] <- tree_prior
  }
  tree_priors
}

#' Initializes a Birth-Death tree prior
#' @param bd_tree_prior a Birth-Death tree prior,
#'   as returned by \code{\link{create_bd_tree_prior}}
#' @param id the index of the first distribution
#' @return an initialized Birth-Death tree prior
#' @author Richel J.C. Bilderbeek
initialize_bd_tree_prior <- function(
  bd_tree_prior,
  id
) {
  testit::assert(is_bd_tree_prior(bd_tree_prior)) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  # birth-rate
  birth_rate_distribution <- get_bd_birth_rate_distr(
    bd_tree_prior)
  birth_rate_distribution$id <- id

  # death rate
  death_rate_distribution <- get_bd_death_rate_distr(
    bd_tree_prior)
  death_rate_distribution$id <- id + 1

  result <- create_bd_tree_prior(
    birth_rate_distribution = birth_rate_distribution,
    death_rate_distribution = death_rate_distribution
  )

  result
}


#' Initializes a Coalescent Constant Population tree prior
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as returned by \code{\link{create_ccp_tree_prior}}
#' @param id the index of the first distribution
#' @return an initialized Coalescent Constant Population tree prior
#' @author Richel J.C. Bilderbeek
initialize_ccp_tree_prior <- function(
  ccp_tree_prior,
  id
) {
  testit::assert(is_ccp_tree_prior(ccp_tree_prior)) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  pop_size_distribution <- get_ccp_pop_size_distr(
    ccp_tree_prior)
  pop_size_distribution$id <- id
  result <- create_ccp_tree_prior(
    pop_size_distribution =  pop_size_distribution
  )
  result
}

#' Initializes a Coalescent Exponential Population tree prior
#' @param cep_tree_prior a Coalescent Exponential Population tree prior,
#'   as returned by \code{\link{create_cep_tree_prior}}
#' @param id the index of the first distribution
#' @return an initialized Coalescent Exponential Population tree prior
#' @author Richel J.C. Bilderbeek
initialize_cep_tree_prior <- function(
  cep_tree_prior,
  id
) {
  testit::assert(is_cep_tree_prior(cep_tree_prior)) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  # pop_size
  pop_size_distribution <- get_cep_pop_size_distr(cep_tree_prior)  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
  pop_size_distribution$id <- id

  # growth rate
  growth_rate_distribution <- get_cep_growth_rate_distr(
    cep_tree_prior)
  growth_rate_distribution$id <- id + 1

  result <- create_cep_tree_prior(
    pop_size_distribution =  pop_size_distribution,
    growth_rate_distribution = growth_rate_distribution
  )

  result
}

#' Initializes a Yule tree prior
#' @param yule_tree_prior a Yule tree prior,
#'   as returned by \code{\link{create_yule_tree_prior}}
#' @param id the index of the first distribution
#' @return an initialized Yule tree prior
#' @author Richel J.C. Bilderbeek
initialize_yule_tree_prior <- function(
  yule_tree_prior,
  id
) {
  testit::assert(is_yule_tree_prior(yule_tree_prior)) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  birth_rate_distribution <- beautier::get_yule_birth_rate_distr(
    yule_tree_prior)
  birth_rate_distribution$id <- id
  result <- create_yule_tree_prior(
    birth_rate_distribution =  birth_rate_distribution
  )

  result
}
