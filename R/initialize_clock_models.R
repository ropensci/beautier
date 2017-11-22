#' Initializes all clock models
#' @param clock_models a list of one or more clock models to be initialized.
#'   Clock priors can be created using \code{\link{create_clock_model}}
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized clock models
#' @author Richel J.C. Bilderbeek
initialize_clock_models <- function(
  clock_models,
  ids,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(length(clock_models) == length(ids))

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))

    if (is_rln_clock_model(clock_model)) {
      # RLN

      if (!is_initialized_rln_clock_model(clock_model)) {

        clock_model <- initialize_rln_clock_model( # nolint internal function call
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        distr_id <- distr_id + 1 # Has one distributions
        param_id <- param_id + beautier::get_distr_n_params(
          clock_model$uclstdev_distr)
      }

    } else {
      testit::assert(beautier::is_strict_clock_model(clock_model))

      if (!is_initialized_strict_clock_model(clock_model)) {

        clock_model <- initialize_strict_clock_model( # nolint internal function call
          clock_model,
          id = ids[i]
        )
        # Does not touch 'distr_id', nor 'param_id'
      }

      testit::assert(is_initialized_strict_clock_model(clock_model)) # nolint internal function call
    }

    clock_models[[i]] <- clock_model
  }
  clock_models
}

#' Initializes a Relaxed Log-Normal clock model
#' @param rln_clock_model a Relaxed Log-Normal clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @inheritParams initialize_clock_models
#' @return an initialized Relaxed Log-Normal clock model
#' @author Richel J.C. Bilderbeek
initialize_rln_clock_model <- function(
  rln_clock_model,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_rln_clock_model(rln_clock_model))

  result <- create_rln_clock_model(
    uclstdev_distr = initialize_distr(
      rln_clock_model$uclstdev_distr,
      distr_id,
      param_id
    )
  )

  result
}

#' Initializes a strict clock model
#' @param strict_clock_model a strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param id the ID of the alignments this clock model is associated with,
#'   as can be extracted from its FASTA filesnames using \code{\link{get_id}})
#' @inheritParams initialize_clock_models
#' @return an initialized strict clock model
#' @author Richel J.C. Bilderbeek
initialize_strict_clock_model <- function(
  strict_clock_model,
  id
) {
  testit::assert(beautier::is_id(id))
  testit::assert(beautier::is_strict_clock_model(strict_clock_model))

  result <- strict_clock_model

  result$clock_rate_parameter$id <- id

  result
}
