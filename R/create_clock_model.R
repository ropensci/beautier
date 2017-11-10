#' General function to create a clock model
#' @note Prefer using the named function
#'   \code{\link{create_rln_clock_model}}
#'   and \code{\link{create_strict_clock_model}}
#' @param name the clock model name. Valid
#'   names can be found in \code{\link{get_clock_model_names}}
#' @param ... specific clock model parameters
#' @return a clock_model
#' @author Richel J.C. Bilderbeek
#' @export
create_clock_model <- function(
  name,
  ...
) {
  if (!is_clock_model_name(name)) {
    clock_models_as_string <- function() {
      s <- NULL
      for (p in get_clock_model_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid clock model name, must be one these: ",
      clock_models_as_string()
    )
  }
  clock_model <- list(name = name, ...)
  clock_model
}

#' Create a relaxed log-normal clock model
#' @param uclstdev_distr the uclstdev distribution,
#'   as created by a \code{\link{create_distribution}} function
#' @return a relaxed log-normal clock_model
#' @export
create_rln_clock_model <- function(
  uclstdev_distr = create_gamma_distr()
) {
  return(
    beautier::create_clock_model(
      name = "relaxed_log_normal",
      uclstdev_distr = uclstdev_distr
    )
  )
}

#' Create a strict clock model
#' @param rate a known clock rate
#' @return a strict clock_model
#' @export
create_strict_clock_model <- function(
  rate = get_default_clock_model_rate()
) {
  return(beautier::create_clock_model(name = "strict", rate = rate))
}
