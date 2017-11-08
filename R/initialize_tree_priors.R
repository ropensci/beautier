#' Initializes all tree priors
#' @param tree_priors a list of one or more tree priors to be initialized.
#'   Tree priors can be created using \code{\link{create_tree_prior}}
#' @return a list of initialized tree priors
#' @author Richel J.C. Bilderbeek
initialize_tree_priors <- function(
  tree_priors
) {
  testit::assert(are_tree_priors(tree_priors))

  id <- 0

  for (i in seq_along(tree_priors)) {
    tree_prior <- tree_priors[[i]]
    testit::assert(beautier::is_tree_prior(tree_prior))

    if (beautier::is_bd_tree_prior(tree_prior)) {

      if (!beautier::is_initialized_bd_tree_prior(tree_prior)) {

        tree_prior <- beautier::initialize_bd_tree_prior(tree_prior, id = id)
        testit::assert(beautier::is_initialized_bd_tree_prior(tree_prior))
        id <- id + 2 # Has two distributions
      }
    } else if (beautier::is_ccp_tree_prior(tree_prior)) {

      if (!beautier::is_initialized_ccp_tree_prior(tree_prior)) {

        tree_prior <- beautier::initialize_ccp_tree_prior(tree_prior, id = id)
        testit::assert(beautier::is_initialized_ccp_tree_prior(tree_prior))
        id <- id + 1 # Has one distribution
      }
    } else if (beautier::is_cep_tree_prior(tree_prior)) {
      if (!beautier::is_initialized_cep_tree_prior(tree_prior)) {

        tree_prior <- beautier::initialize_cep_tree_prior(tree_prior, id = id)
        testit::assert(beautier::is_initialized_cep_tree_prior(tree_prior))
        id <- id + 2 # Has two distribution
      }
    } else if (beautier::is_yule_tree_prior(tree_prior)) {

      if (!beautier::is_initialized_yule_tree_prior(tree_prior)) {

        tree_prior <- beautier::initialize_yule_tree_prior(tree_prior, id = id)
        testit::assert(beautier::is_initialized_yule_tree_prior(tree_prior))
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
  testit::assert(is_bd_tree_prior(bd_tree_prior))

  # birth-rate
  birth_rate_distribution <- beautier::get_bd_birth_rate_distr(
    bd_tree_prior)
  testit::assert(beautier::is_distribution(birth_rate_distribution))
  testit::assert("id" %in% names(birth_rate_distribution))
  birth_rate_distribution$id <- id

  # death rate
  death_rate_distribution <- beautier::get_bd_death_rate_distr(
    bd_tree_prior)
  testit::assert(beautier::is_distribution(death_rate_distribution))
  testit::assert("id" %in% names(death_rate_distribution))
  death_rate_distribution$id <- id + 1

  result <- beautier::create_bd_tree_prior(
    birth_rate_distribution = birth_rate_distribution,
    death_rate_distribution = death_rate_distribution
  )
  testit::assert(beautier::is_initialized_bd_tree_prior(result))
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
  testit::assert(is_ccp_tree_prior(ccp_tree_prior))

  pop_size_distribution <- beautier::get_ccp_pop_size_distr(
    ccp_tree_prior)
  testit::assert(beautier::is_distribution(pop_size_distribution))
  testit::assert("id" %in% names(pop_size_distribution))
  pop_size_distribution$id <- id
  result <- beautier::create_ccp_tree_prior(
    pop_size_distribution =  pop_size_distribution
  )
  testit::assert(beautier::is_initialized_ccp_tree_prior(result))
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
  testit::assert(is_cep_tree_prior(cep_tree_prior))

  # pop_size
  pop_size_distribution <- beautier::get_cep_pop_size_distr(cep_tree_prior)
  testit::assert(beautier::is_distribution(pop_size_distribution))
  testit::assert("id" %in% names(pop_size_distribution))
  pop_size_distribution$id <- id

  # growth rate
  growth_rate_distribution <- beautier::get_cep_growth_rate_distr(
    cep_tree_prior)
  testit::assert(beautier::is_distribution(growth_rate_distribution))
  testit::assert("id" %in% names(growth_rate_distribution))
  growth_rate_distribution$id <- id + 1

  result <- beautier::create_cep_tree_prior(
    pop_size_distribution =  pop_size_distribution,
    growth_rate_distribution = growth_rate_distribution
  )
  testit::assert(beautier::is_initialized_cep_tree_prior(result))
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
  testit::assert(is_yule_tree_prior(yule_tree_prior))

  birth_rate_distribution <- beautier::get_yule_birth_rate_distr(
    yule_tree_prior)
  testit::assert(beautier::is_distribution(birth_rate_distribution))
  testit::assert("id" %in% names(birth_rate_distribution))
  birth_rate_distribution$id <- id
  result <- beautier::create_yule_tree_prior(
    birth_rate_distribution =  birth_rate_distribution
  )
  testit::assert(beautier::is_initialized_yule_tree_prior(result))
  result
}
