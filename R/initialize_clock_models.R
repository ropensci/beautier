#' Initializes all clock models
#' @param clock_models a list of one or more clock models to be initialized.
#'   Clock priors can be created using \code{\link{create_clock_model}}
#' @param id the first distributions' ID
#' @return a list of initialized clock models
#' @author Richel J.C. Bilderbeek
initialize_clock_models <- function(
  clock_models,
  id = 0
) {
  testit::assert(beautier::are_clock_models(clock_models))

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))

    if (is_rln_clock_model(clock_model)) {

      if (!is_initialized_rln_clock_model(clock_model)) {

        clock_model <- initialize_rln_clock_model(clock_model, id = id)  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        testit::assert(is_initialized_rln_clock_model(clock_model))  # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is
        id <- id + 1 # Has one distributions
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
#' @param id the index of the first distribution
#' @return an initialized Birth-Death clock model
#' @author Richel J.C. Bilderbeek
initialize_rln_clock_model <- function(
  rln_clock_model,
  id
) {
  testit::assert(is_rln_clock_model(rln_clock_model)) # nolint one day I will find out why 'create_beast2_input_data' is no problem, and this internal function call is

  # uclstdev_distribution
  uclstdev_distribution <- rln_clock_model$uclstdev_distribution
  uclstdev_distribution$id <- id

  result <- create_rln_clock_model(
    uclstdev_distribution = uclstdev_distribution
  )

  result
}
