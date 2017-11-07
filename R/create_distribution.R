#' General function to create a distribution.
#' @param name the distribution name. Valid
#'   names can be found in \code{\link{get_distribution_names}}
#' @param ... specific distribution parameters
#' @note Prefer using the
#'   named functions \code{\link{create_uniform_distribution}},
#'   and \code{\link{create_normal_distribution}}
#' @return a distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_distribution <- function(
  name,
  ...
) {
  if (!is_distribution_name(name)) {
    distributions_as_string <- function() {
      s <- NULL
      for (p in get_distribution_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid distribution name, must be one these: ",
      distributions_as_string()
    )
  }
  distribution <- list(
    name = name,
    ...
  )
  distribution
}

#' Create a uniform distribution
#' @inheritParams create_distribution
#' @return a uniform distribution
#' @author Richel J.C. Bilderbeek
#' @export
create_uniform_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "uniform"
    )
  )
}

#' Create an normal distribution
#' @inheritParams create_distribution
#' @return a normal distribution
#' @export
create_normal_distribution <- function(
) {
  return(
    beautier::create_distribution(
      name = "normal"
    )
  )
}
