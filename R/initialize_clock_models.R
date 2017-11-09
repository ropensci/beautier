#' Initializes all clock models
#' @param clock_models a list of one or more clock models to be initialized.
#'   Clock priors can be created using \code{\link{create_clock_model}}
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized clock models
#' @author Richel J.C. Bilderbeek
initialize_clock_models <- function(
  clock_models,
  distr_id = 0,
  param_id = 0
) {
  id <- distr_id # Notational convenience
  testit::assert(beautier::are_clock_models(clock_models))

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))

    if (is_rln_clock_model(clock_model)) {

      if (!is_initialized_rln_clock_model(clock_model)) {

        clock_model <- initialize_rln_clock_model( # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        distr_id <- distr_id + 1 # Has one distributions
        param_id <- param_id + get_distr_n_params(clock_model$uclstdev_distribution)
      }
    } else if (is_strict_clock_model(clock_model)) {

      # Always initialized
    }

    clock_models[[i]] <- clock_model
  }
  clock_models
}

#' Initializes a Birth-Death clock model
#' @param rln_clock_model a Birth-Death clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @inheritParams initialize_clock_models
#' @return an initialized Birth-Death clock model
#' @author Richel J.C. Bilderbeek
initialize_rln_clock_model <- function(
  rln_clock_model,
  distr_id,
  param_id
) {
  testit::assert(is_rln_clock_model(rln_clock_model)) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  # uclstdev_distribution
  result <- create_rln_clock_model(
    uclstdev_distribution = initialize_distribution(
      rln_clock_model$uclstdev_distribution,
      distr_id,
      param_id
    )
  )

  result
}
