#' Internal function to create a tree prior
#' @note Prefer the use the named functions
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}}
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#'   instead
#' @param name the tree prior name. Can be any name
#'   in \code{\link{get_tree_prior_names}}
#' @param ... specific tree prior parameters
#' @return a tree_prior
#' @author Richel J.C. Bilderbeek
#' @export
create_tree_prior <- function(
  name,
  ...
) {
  if (!is_tree_prior_name(name)) {
    tree_priors_as_string <- function() {
      s <- NULL
      for (p in get_tree_prior_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid tree prior name, must be one these: ",
      tree_priors_as_string()
    )
  }
  tree_prior <- list(name = name, ...)
  tree_prior
}


#' Create a Yule tree prior
#' @param birth_rate_distribution the birth rate distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @return a Yule tree_prior
#' @usage
#' create_yule_tree_prior(
#'   birth_rate_distribution = beautier::create_uniform_distr()
#' )
#' @author Richel J.C. Bilderbeek
#' @export
create_yule_tree_prior <- function(
  birth_rate_distribution = beautier::create_uniform_distr()
) {
  return(
    beautier::create_tree_prior(
      name = "yule",
      birth_rate_distribution = birth_rate_distribution
    )
  )
}

#' Create a Birth-Death tree prior
#' @return a Birth-Death tree_prior
#' @param birth_rate_distribution the birth rate distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @param death_rate_distribution the death rate distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @return a Birth-Death tree_prior
#' @usage
#' create_bd_tree_prior(
#'   birth_rate_distribution = beautier::create_uniform_distr(),
#'   death_rate_distribution = beautier::create_uniform_distr()
#' )
#' @author Richel J.C. Bilderbeek
#' @export
create_bd_tree_prior <- function(
  birth_rate_distribution = beautier::create_uniform_distr(),
  death_rate_distribution = beautier::create_uniform_distr()
  ) {
  return(
    beautier::create_tree_prior(
      name = "birth_death",
      birth_rate_distribution = birth_rate_distribution,
      death_rate_distribution = death_rate_distribution
    )
  )
}

#' Create a Coalescent Bayesian Skyline tree prior
#' @return a Coalescent Bayesian Skyline tree_prior
#' @export
create_cbs_tree_prior <- function() {
  return(beautier::create_tree_prior(name = "coalescent_bayesian_skyline"))
}

#' Create a Coalescent Constant Population tree prior
#' @param pop_size_distribution the population distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @return a Coalescent Constant Population tree_prior
#' @export
create_ccp_tree_prior <- function(
  pop_size_distribution = beautier::create_one_div_x_distr()
) {
  return(
    beautier::create_tree_prior(
      name = "coalescent_constant_population",
      pop_size_distribution = pop_size_distribution
    )
  )
}

#' Create a Coalescent Exponential Population tree prior
#' @param pop_size_distribution the population distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @param growth_rate_distribution the growth rate distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @return a Coalescent Exponential Population tree_prior
#' @export
create_cep_tree_prior <- function(
  pop_size_distribution = create_one_div_x_distr(),
  growth_rate_distribution = create_laplace_distr()
) {
  return(
    beautier::create_tree_prior(
      name = "coalescent_exponential_population",
      pop_size_distribution = pop_size_distribution,
      growth_rate_distribution = growth_rate_distribution
    )
  )
}
