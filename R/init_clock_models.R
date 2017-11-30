#' Initializes all clock models
#' @param clock_models a list of one or more clock models to be initialized.
#'   Clock priors can be created using \code{\link{create_clock_model}}
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized clock models
#' @author Richel J.C. Bilderbeek
init_clock_models <- function(
  clock_models,
  ids,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(length(clock_models) <= length(ids))

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))

    if (is_rln_clock_model(clock_model)) {
      # RLN

      if (!is_init_rln_clock_model(clock_model)) {

        clock_model <- init_rln_clock_model( # nolint internal function call
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        distr_id <- distr_id  # Has one distributions
        param_id <- param_id + beautier::get_distr_n_params(
          clock_model$ucldstdev_distr) +
          1 # mparam
      }

    } else {
      testit::assert(beautier::is_strict_clock_model(clock_model))

      if (!is_init_strict_clock_model(clock_model)) {

        clock_model <- init_strict_clock_model( # nolint internal function call
          clock_model,
          id = ids[i]
        )
        # Does not touch 'distr_id', nor 'param_id'
      }

      testit::assert(is_init_strict_clock_model(clock_model)) # nolint internal function call
    }

    if (is.na(clock_model$id)) clock_model$id <- ids[i]

    clock_models[[i]] <- clock_model
  }
  clock_models
}

#' Initializes a Relaxed Log-Normal clock model
#' @param rln_clock_model a Relaxed Log-Normal clock model,
#'   as returned by \code{\link{create_rln_clock_model}}
#' @inheritParams init_clock_models
#' @return an initialized Relaxed Log-Normal clock model
#' @author Richel J.C. Bilderbeek
init_rln_clock_model <- function(
  rln_clock_model,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_rln_clock_model(rln_clock_model))

  result <- create_rln_clock_model(
    ucldstdev_distr = init_distr(
      rln_clock_model$ucldstdev_distr,
      distr_id,
      param_id
    ),
    mean_clock_rate = rln_clock_model$mean_clock_rate,
    n_rate_categories = rln_clock_model$n_rate_categories,
    normalize_mean_clock_rate = rln_clock_model$normalize_mean_clock_rate
  )

  if (is.na(result$mparam_id)) {
    result$mparam_id <- param_id +
      get_distr_n_params(rln_clock_model$ucldstdev_distr)
  }

  result
}

#' Initializes a strict clock model
#' @param strict_clock_model a strict clock model,
#'   as returned by \code{\link{create_strict_clock_model}}
#' @param id the ID of the alignments this clock model is associated with,
#'   as can be extracted from its FASTA filesnames using \code{\link{get_id}})
#' @inheritParams init_clock_models
#' @return an initialized strict clock model
#' @author Richel J.C. Bilderbeek
init_strict_clock_model <- function(
  strict_clock_model,
  id
) {
  testit::assert(beautier::is_id(id))
  testit::assert(beautier::is_strict_clock_model(strict_clock_model))

  result <- strict_clock_model

  result$clock_rate_param$id <- id

  result
}
