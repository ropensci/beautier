#' General function to create a tree prior. Prefer the use the named
#' functions \code{\link{create_yule_tree_prior}},
#' \code{\link{create_bd_tree_prior}}, \code{\link{create_cbs_tree_prior}},
#' and \code{\link{create_ccp_tree_prior}}
#' @param name the tree prior name. Can be any name
#'   in \code{\link{get_tree_prior_names}}
#' @return a tree_prior
#' @author Richel J.C. Bilderbeek
#' @export
create_tree_prior <- function(
  name
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
  tree_prior <- list(name = name)
  tree_prior
}


#' Create a Yule tree prior
#' @return a Yule tree_prior
#' @export
create_yule_tree_prior <- function() {
  return(beastscriptr::create_tree_prior(name = "yule"))
}

#' Create a Birth-Death tree prior
#' @return a Birth-Death tree_prior
#' @export
create_bd_tree_prior <- function() {
  return(beastscriptr::create_tree_prior(name = "birth_death"))
}

#' Create a Coalescent Bayesian Skyline tree prior
#' @return a Coalescent Bayesian Skyline tree_prior
#' @export
create_cbs_tree_prior <- function() {
  return(beastscriptr::create_tree_prior(name = "coalescent_bayesian_skyline"))
}

#' Create a Coalescent Constant Population tree prior
#' @return a Coalescent Constant Population tree_prior
#' @export
create_ccp_tree_prior <- function() {
  return(beastscriptr::create_tree_prior(
    name = "coalescent_constant_population"))
}
