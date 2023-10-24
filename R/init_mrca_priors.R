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
  param_id = 0,
  beauti_options
) {
  if (length(mrca_priors) == 1 && is_one_na(mrca_priors)) return(NA)

  check_true(length(mrca_priors) == 1)
  check_true(are_mrca_priors(mrca_priors))
  if (beauti_options$beast2_version == "2.4") {
    names <- "auto_name_1"
  } else {
    names <- "ingroup"
  }

  for (i in seq_along(mrca_priors)) {
    mrca_prior <- mrca_priors[[i]]
    check_true(is_mrca_prior(mrca_prior))

    if (is_one_na(mrca_prior$name)) {
      mrca_prior$name <- names[i]
    }
    if (is_one_na(mrca_prior$clock_prior_distr_id)) {
      mrca_prior$clock_prior_distr_id <- distr_id
      distr_id <- distr_id + 1
    }
    if (is_distr(mrca_prior$mrca_distr) &&
        !is_init_distr(mrca_prior$mrca_distr)
    ) {
      mrca_prior$mrca_distr <- init_distr(
        distr = mrca_prior$mrca_distr,
        distr_id = distr_id,
        param_id = param_id
      )
      distr_id <- distr_id + 1
      param_id <- param_id + get_distr_n_params(mrca_prior$mrca_distr)
    }

    check_true(is_mrca_prior(mrca_prior))
    mrca_priors[[i]] <- mrca_prior
  }
  mrca_priors
}
