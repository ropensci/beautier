#' General function to create a clock model.
#' Prefer using 'create_rln_clock_model' and 'create_strict_clock_model'
#' @param name the clock model name. Valid
#'   names can be found in 'get_clock_model_names'
#' @return a clock_model
#' @export
create_clock_model <- function(
  name
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
    stop("invalid clock model name")
  }
  clock_model <- list(name = name)
  clock_model
}

#' Create a relaxed log-normal clock model
#' @return a relaxed log-normal clock_model
#' @export
create_rln_clock_model <- function() {
  return(create_clock_model(name = "relaxed_log_normal"))
}

#' Create a strict clock model
#' @param rate a known clock rate
#' @return a strict clock_model
#' @export
create_strict_clock_model <- function(
  rate = get_default_clock_model_rate()
) {
  return(list(name = "strict", rate = rate))
}
