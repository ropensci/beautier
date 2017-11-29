#' Initializes all tree priors
#' @param tree_priors a list of one or more tree priors to be initialized.
#'   Tree priors can be created using \code{\link{create_tree_prior}}
#' @param ids the IDs of the alignments (can be extracted from
#'   their FASTA filesnames using \code{\link{get_ids}})
#' @param distr_id the first distributions' ID
#' @param param_id the first parameter's ID
#' @return a list of initialized tree priors
#' @author Richel J.C. Bilderbeek
init_tree_priors <- function(
  tree_priors,
  ids,
  distr_id = 0,
  param_id = 0
) {
  testit::assert(beautier::are_tree_priors(tree_priors))

  for (i in seq_along(tree_priors)) {
    tree_prior <- tree_priors[[i]]
    testit::assert(beautier::is_tree_prior(tree_prior))

    if (is_bd_tree_prior(tree_prior)) {
      if (!is_init_bd_tree_prior(tree_prior)) {
        tree_prior <- init_bd_tree_prior( # nolint internal function call
          tree_prior, distr_id = distr_id, param_id = param_id
        )
      }
    } else if (is_cbs_tree_prior(tree_prior)) {
      # Nothing to do
    } else if (is_ccp_tree_prior(tree_prior)) {
      if (!is_init_ccp_tree_prior(tree_prior)) {
        tree_prior <- init_ccp_tree_prior( # nolint internal function call
          tree_prior, distr_id = distr_id, param_id = param_id
        )
      }
    } else if (is_cep_tree_prior(tree_prior)) {
      if (!is_init_cep_tree_prior(tree_prior)) {
        tree_prior <- init_cep_tree_prior( # nolint internal function call
          tree_prior, distr_id = distr_id, param_id = param_id
        )
      }
    } else {
      testit::assert(beautier::is_yule_tree_prior(tree_prior))
      if (!is_init_yule_tree_prior(tree_prior)) {
        tree_prior <- init_yule_tree_prior( # nolint internal function call
          tree_prior, distr_id = distr_id, param_id = param_id
        )
      }
    }
    distr_id <- distr_id + beautier::get_tree_prior_n_distrs(tree_prior)
    param_id <- param_id + beautier::get_tree_prior_n_params(tree_prior)

    if (is.na(tree_prior$id)) tree_prior$id <- ids[i]
    tree_priors[[i]] <- tree_prior
  }
  tree_priors
}

#' Initializes a Birth-Death tree prior
#' @inheritParams init_tree_priors
#' @param bd_tree_prior a Birth-Death tree prior,
#'   as returned by \code{\link{create_bd_tree_prior}}
#' @return an initialized Birth-Death tree prior
#' @author Richel J.C. Bilderbeek
init_bd_tree_prior <- function(
  bd_tree_prior,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_bd_tree_prior(bd_tree_prior))

  result <- create_bd_tree_prior(
    birth_rate_distr = init_distr(
      bd_tree_prior$birth_rate_distr,
      distr_id,
      param_id
    ),
    death_rate_distr = init_distr(
      bd_tree_prior$death_rate_distr,
      distr_id + 1,
      param_id + get_distr_n_params(bd_tree_prior$birth_rate_distr)
    )
  )

  result
}


#' Initializes a Coalescent Constant Population tree prior
#' @inheritParams init_tree_priors
#' @param ccp_tree_prior a Coalescent Constant Population tree prior,
#'   as returned by \code{\link{create_ccp_tree_prior}}
#' @return an initialized Coalescent Constant Population tree prior
#' @author Richel J.C. Bilderbeek
init_ccp_tree_prior <- function(
  ccp_tree_prior,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_ccp_tree_prior(ccp_tree_prior))

  result <- create_ccp_tree_prior(
    pop_size_distr = init_distr(
      ccp_tree_prior$pop_size_distr,
      distr_id,
      param_id
    )
  )
  result
}

#' Initializes a Coalescent Exponential Population tree prior
#' @inheritParams init_tree_priors
#' @param cep_tree_prior a Coalescent Exponential Population tree prior,
#'   as returned by \code{\link{create_cep_tree_prior}}
#' @return an initialized Coalescent Exponential Population tree prior
#' @author Richel J.C. Bilderbeek
init_cep_tree_prior <- function(
  cep_tree_prior,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_cep_tree_prior(cep_tree_prior))
  testit::assert(!is.na(distr_id))
  testit::assert(!is.na(param_id))
  testit::assert(!is.na(
    beautier::get_distr_n_params(cep_tree_prior$pop_size_distr)))

  result <- create_cep_tree_prior(
    pop_size_distr = init_distr(
      cep_tree_prior$pop_size_distr,
      distr_id,
      param_id
    ),
    growth_rate_distr = init_distr(
      cep_tree_prior$growth_rate_distr,
      distr_id + 1,
      param_id + get_distr_n_params(cep_tree_prior$pop_size_distr)
    )
  )

  result
}

#' Initializes a Yule tree prior
#' @inheritParams init_tree_priors
#' @param yule_tree_prior a Yule tree prior,
#'   as returned by \code{\link{create_yule_tree_prior}}
#' @return an initialized Yule tree prior
#' @author Richel J.C. Bilderbeek
init_yule_tree_prior <- function(
  yule_tree_prior,
  distr_id,
  param_id
) {
  testit::assert(beautier::is_yule_tree_prior(yule_tree_prior))

  result <- create_yule_tree_prior(
    birth_rate_distr = init_distr(
      yule_tree_prior$birth_rate_distr,
      distr_id,
      param_id
    )
  )

  result
}
