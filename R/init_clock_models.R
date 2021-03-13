#' Initializes all clock models
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized clock models
#' @author Richèl J.C. Bilderbeek
#' @export
init_clock_models <- function(
  fasta_filenames,
  clock_models,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(all(file.exists(fasta_filenames)))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(length(clock_models) == length(fasta_filenames))
  ids <- beautier::get_alignment_ids_from_fasta_filenames(fasta_filenames)
  n_taxa <- beautier::get_n_taxa(fasta_filenames[1])

  for (i in seq_along(clock_models)) {
    clock_model <- clock_models[[i]]
    testit::assert(beautier::is_clock_model(clock_model))

    if (beautier::is_rln_clock_model(clock_model)) {
      # RLN

      if (!beautier::is_init_rln_clock_model(clock_model)) {

        clock_model <- beautier::init_rln_clock_model(
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        if (beautier::is_one_na(clock_model$dimension)) {
          clock_model$dimension <- (2 * n_taxa) - 2
        }

        distr_id <- distr_id  + 2 # Has two distributions
        param_id <- param_id + beautier::get_distr_n_params(
          clock_model$ucldstdev_distr) +
          1 # mparam
      }

    } else {
      testit::assert(beautier::is_strict_clock_model(clock_model))

      if (!beautier::is_init_strict_clock_model(clock_model)) {

        clock_model <- beautier::init_strict_clock_model(
          clock_model,
          distr_id = distr_id,
          param_id = param_id
        )
        distr_id <- distr_id  + 1 # Has one distributions
        param_id <- param_id + beautier::get_distr_n_params(
          clock_model$clock_rate_distr)
      }

      testit::assert(beautier::is_init_strict_clock_model(clock_model))
    }

    if (beautier::is_one_na(clock_model$id)) clock_model$id <- ids[i]

    clock_models[[i]] <- clock_model
  }
  clock_models
}

#' Initializes a Relaxed Log-Normal clock model
#' @inheritParams default_params_doc
#' @return an initialized Relaxed Log-Normal clock model
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' rln_clock_model <- create_rln_clock_model()
#' # FALSE: not yet initialized
#' is_init_rln_clock_model(rln_clock_model)
#' rln_clock_model <- init_rln_clock_model(rln_clock_model)
#' # Dimension is set to NA by default, for unknown reasons.
#' # Because 'init_rln_clock_model' does not initialize it (for
#' # unknown reasons), set it manually
#' rln_clock_model$dimension <- 42
#' # TRUE: now it is initialized
#' is_init_rln_clock_model(rln_clock_model)
#' @export
init_rln_clock_model <- function(
  rln_clock_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_rln_clock_model(rln_clock_model))
  ucldstdev_distr <- beautier::init_distr(
    rln_clock_model$ucldstdev_distr,
    distr_id,
    param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + beautier::get_distr_n_params(ucldstdev_distr)
  mean_rate_prior_distr <- beautier::init_distr(
    rln_clock_model$mean_rate_prior_distr,
    distr_id,
    param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + beautier::get_distr_n_params(mean_rate_prior_distr)

  mparam_id <- rln_clock_model$mparam_id
  if (beautier::is_one_na(mparam_id)) {
    mparam_id <- param_id
    param_id <- param_id + 1
  }

  result <- beautier::create_rln_clock_model(
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
#'
#' strict_clock_model <- create_strict_clock_model()
#' # FALSE: not yet initialized
#' is_init_strict_clock_model(strict_clock_model)
#' strict_clock_model <- init_strict_clock_model(strict_clock_model)
#' # TRUE: initialized
#' is_init_strict_clock_model(strict_clock_model)
#' @export
init_strict_clock_model <- function(
  strict_clock_model,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::is_strict_clock_model(strict_clock_model))

  # clock_rate_distr
  strict_clock_model$clock_rate_distr <- beautier::init_distr(
    strict_clock_model$clock_rate_distr,
    distr_id,
    param_id
  )
  distr_id <- distr_id + 1
  param_id <- param_id + beautier::get_distr_n_params(
    strict_clock_model$clock_rate_distr)

  # clock_rate_param
  strict_clock_model$clock_rate_param$id <- param_id
  param_id <- param_id + 1

  strict_clock_model
}
