#' Check if the clock model is a valid clock model.
#'
#' Calls \code{stop} if the clock model is invalid
#' @inheritParams default_params_doc
#' @return TRUE if \code{clock_model} is a valid clock model
#' @seealso Use \link{create_clock_model} to create a valid clock model
#' @examples
#' check_clock_model(create_strict_clock_model())
#' check_clock_model(create_rln_clock_model())
#' @author Richèl J.C. Bilderbeek
#' @export
check_clock_model <- function(clock_model) {
  argument_names <- c(
    "id",
    "name"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(clock_model)) {
      stop(
        "'", arg_name, "' must be an element of 'clock_model'. ",
        "Tip: use 'create_clock_model'"
      )
    }
  }
  if (!clock_model$name %in% beautier::get_clock_model_names()) {
    stop(
      "'clock_model$name' must be one of the clock model names (",
      paste0(beautier::get_clock_model_names(), collapse = ", "), "). \n",
      "Actual value: ", clock_model$name
    )
  }
  if (clock_model$name == "strict") {
    beautier::check_strict_clock_model(clock_model)
  } else {
    testit::assert(clock_model$name == "relaxed_log_normal")
    beautier::check_rln_clock_model(clock_model)
  }
}

#' Check if the clock model is a valid clock model.
#'
#' Calls \code{stop} if the clock model is invalid
#' @inheritParams default_params_doc
#' @return TRUE if \code{clock_model} is a valid clock model
#' @seealso Use \link{create_clock_model} to create a valid clock model
#' @examples
#' check_rln_clock_model(create_rln_clock_model())
#' @author Richèl J.C. Bilderbeek
#' @export
check_rln_clock_model <- function(clock_model) {
  argument_names <- c(
    "name", "id", "ucldstdev_distr", "mean_rate_prior_distr", "mparam_id",
    "mean_clock_rate", "n_rate_categories", "normalize_mean_clock_rate",
    "dimension"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(clock_model)) {
      stop(
        "'", arg_name, "' must be an element of a ",
        "relaxed log-normal clock model. \n",
        "Tip: use 'create_rln_clock_model'"
      )
    }
  }
  if (clock_model$name != "relaxed_log_normal") {
    stop(
      "clock_model$name must be 'relaxed_log_normal' for a relaxed ",
      "log-normal clock model. \n",
      "Tip: use create_rln_clock_model. \n",
      "Actual value: ", clock_model$name
    )
  }
  if (!beautier::is_distr(clock_model$ucldstdev_distr)) {
    stop(
      "'clock_model$ucldstdev_distr' must be a distribution. \n",
      "Tip: use create_distr. \n",
      "Actual value: ", clock_model$ucldstdev_distr
    )
  }
  if (!beautier::is_distr(clock_model$mean_rate_prior_distr)) {
    stop(
      "'clock_model$mean_rate_prior_distr' must be a distribution. \n",
      "Tip: use create_distr. \n",
      "Actual value: ", clock_model$mean_rate_prior_distr
    )
  }
}

#' Check if the clock model is a valid clock model.
#'
#' Calls \code{stop} if the clock model is invalid
#' @inheritParams default_params_doc
#' @return TRUE if \code{clock_model} is a valid clock model
#' @seealso Use \link{create_clock_model} to create a valid clock model
#' @examples
#'  check_strict_clock_model(create_strict_clock_model())
#' @author Richèl J.C. Bilderbeek
#' @export
check_strict_clock_model <- function(clock_model) {
  argument_names <- c(
    "name", "id", "clock_rate_param", "clock_rate_distr"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(clock_model)) {
      stop(
        "'", arg_name, "' must be an element of a ",
        "strict clock model. \n",
        "Tip: use 'create_strict_clock_model'"
      )
    }
  }
  if (clock_model$name != "strict") {
    stop(
      "clock_model$name must be 'strict' for a strict ",
      "clock model. \n",
      "Tip: use create_strict_clock_model. \n",
      "Actual value: ", clock_model$name
    )
  }
  if (!beautier::is_distr(clock_model$clock_rate_distr)) {
    stop(
      "'clock_model$clock_rate_distr' must be a distribution. \n",
      "Tip: use create_distr. \n",
      "Actual value: ", clock_model$clock_rate_distr
    )
  }
  if (!beautier::is_param(clock_model$clock_rate_param)) {
    stop(
      "'clock_model$clock_rate_param' must be a parameter. \n",
      "Tip: use create_param. \n",
      "Actual value: ", clock_model$clock_rate_param
    )
  }
}
