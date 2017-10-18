#' Create a tree prior
#' @param name the tree prior name. Can be 'birth_death'
#'   or 'coalescent_constant_population'
#' @return a site_model
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
