#' Initializes all MRCA priors
#' @inheritParams default_params_doc
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized MRCA priors
#' @author Richèl J.C. Bilderbeek
#' @export
init_mrca_priors <- function(
  mrca_priors,
  distr_id = 0,
  param_id = 0
) {
  if (length(mrca_priors) == 1 && beautier::is_one_na(mrca_priors)) return(NA)
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  names <- paste0("auto_name_", seq_along(mrca_priors))

  for (i in seq_along(mrca_priors)) {
    mrca_prior <- mrca_priors[[i]]
    testit::assert(beautier::is_mrca_prior(mrca_prior))

    if (beautier::is_one_na(mrca_prior$name)) {
      mrca_prior$name <- names[i]
    }
    if (beautier::is_one_na(mrca_prior$clock_prior_distr_id)) {
      mrca_prior$clock_prior_distr_id <- distr_id
      distr_id <- distr_id + 1
    }
    if (beautier::is_distr(mrca_prior$mrca_distr) &&
        !beautier::is_init_distr(mrca_prior$mrca_distr)
    ) {
      mrca_prior$mrca_distr <- beautier::init_distr(
        distr = mrca_prior$mrca_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + beautier::get_distr_n_params(mrca_prior$mrca_distr)
    }

    testit::assert(beautier::is_mrca_prior(mrca_prior))
    mrca_priors[[i]] <- mrca_prior
  }
  mrca_priors
}
