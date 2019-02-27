#' Initializes all clock models
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized clock models
#' @author Richèl J.C. Bilderbeek
#' @noRd
init_clock_models <- function(
  fasta_filenames,
  clock_models,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(files_exist(fasta_filenames)) # nolint beautier function
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  testit::assert(length(clock_models) == length(fasta_filenames))
  ids <- get_alignment_ids(fasta_filenames) # nolint beautier function
  n_taxa <- get_n_taxa(fasta_filenames[1]) # nolint beautier function

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(is_clock_model(clock_model)) # nolint beautier function

    if (is_rln_clock_model(clock_model)) { # nolint beautier function
      # RLN

      if (!is_init_rln_clock_model(clock_model)) { # nolint beautier function

        clock_model <- init_rln_clock_model( # nolint beautier function call
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        if (is_one_na(clock_model$dimension)) { # nolint beautier function
          clock_model$dimension <- (2 * n_taxa) - 2
        }

        distr_id <- distr_id  + 2 # Has two distributions
        param_id <- param_id + get_distr_n_params( # nolint beautier function
          clock_model$ucldstdev_distr) +
          1 # mparam
      }

    } else {
      testit::assert(is_strict_clock_model(clock_model)) # nolint beautier function

      if (!is_init_strict_clock_model(clock_model)) { # nolint beautier function

        clock_model <- init_strict_clock_model( # nolint beautier function call
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        distr_id <- distr_id  + 1 # Has one distributions
        param_id <- param_id + get_distr_n_params( # nolint beautier function
          clock_model$clock_rate_distr)
      }

      testit::assert(is_init_strict_clock_model(clock_model)) # nolint beautier function call
    }

    if (is_one_na(clock_model$id)) clock_model$id <- ids[i] # nolint beautier function

    clock_models[[i]] <- clock_model
  }
  clock_models
}

#' Initializes a Relaxed Log-Normal clock model
#' @inheritParams default_params_doc
#' @return an initialized Relaxed Log-Normal clock model
#' @author Richèl J.C. Bilderbeek
init_rln_clock_model <- function(
  rln_clock_model,
  distr_id,
  param_id
) {
  testit::assert(is_rln_clock_model(rln_clock_model)) # nolint beautier function
  ucldstdev_distr <- init_distr( # nolint beautier function
    rln_clock_model$ucldstdev_distr,
    distr_id,
    param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + get_distr_n_params(ucldstdev_distr) # nolint beautier function
  mean_rate_prior_distr <- init_distr( # nolint beautier function
    rln_clock_model$mean_rate_prior_distr,
    distr_id,
    param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + get_distr_n_params(mean_rate_prior_distr) # nolint beautier function

  mparam_id <- rln_clock_model$mparam_id
  if (is_one_na(mparam_id)) { # nolint beautier function
    mparam_id <- param_id
    param_id <- param_id + 1
  }

  result <- create_rln_clock_model(
    id = rln_clock_model$id,
    ucldstdev_distr = ucldstdev_distr,
    mean_rate_prior_distr = mean_rate_prior_distr,
    mean_clock_rate = rln_clock_model$mean_clock_rate,
    n_rate_categories = rln_clock_model$n_rate_categories,
    normalize_mean_clock_rate = rln_clock_model$normalize_mean_clock_rate,
    mparam_id = mparam_id
  )


  result
}

#' Initializes a strict clock model
#' @inheritParams default_params_doc
#' @return an initialized strict clock model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   strict_clock_model <- create_strict_clock_model()
init_strict_clock_model <- function(
  strict_clock_model,
  distr_id,
  param_id
) {
  testit::assert(is_strict_clock_model(strict_clock_model)) # nolint beautier function

  # clock_rate_distr
  strict_clock_model$clock_rate_distr <- init_distr( # nolint beautier function
    strict_clock_model$clock_rate_distr,
    distr_id,
    param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + get_distr_n_params( # nolint beautier function
    strict_clock_model$clock_rate_distr)

  # clock_rate_param
  strict_clock_model$clock_rate_param$id <- param_id
  param_id <- param_id + 1

  strict_clock_model
}
