#' Initializes all MRCA priors
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized MRCA priors
#' @author Richèl J.C. Bilderbeek
#' @noRd
init_mrca_priors <- function(
  mrca_priors,
  distr_id = 0,
  param_id = 0
) {
  if (length(mrca_priors) == 1 && is_one_na(mrca_priors)) return(NA) # nolint beautier function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  names <- paste0("auto_name_", seq_along(mrca_priors))

  for (i in seq_along(mrca_priors)) {
    mrca_prior <- mrca_priors[[i]]
    testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function

    if (is_one_na(mrca_prior$name)) { # nolint beautier function
      mrca_prior$name <- names[i]
    }
    if (is_one_na(mrca_prior$clock_prior_distr_id)) { # nolint beautier function
      mrca_prior$clock_prior_distr_id <- distr_id
      distr_id <- distr_id + 1
    }
    if (is_distr(mrca_prior$mrca_distr) && # nolint beautier function
        !is_init_distr(mrca_prior$mrca_distr) # nolint beautier function
    ) {
      mrca_prior$mrca_distr <- init_distr( # nolint beautier function
        distr = mrca_prior$mrca_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + get_distr_n_params(mrca_prior$mrca_distr) # nolint beautier function
    }

    testit::assert(is_mrca_prior(mrca_prior)) # nolint beautier function
    mrca_priors[[i]] <- mrca_prior
  }
  mrca_priors
}
