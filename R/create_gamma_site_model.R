#' Create a gamma site model, part of a site model
#' @param gamma_cat_count the number of gamma categories, must
#'   be an integer with value zero or more
#' @param gamma_shape gamma curve shape parameter
#' @param prop_invariant the proportion invariant, must be a value
#'   from 0.0 to 1.0
#' @param gamma_shape_prior_distr the distribution of the gamma shape prior
#' @return a gamma site model
#' @note Use \code{\link{create_site_model}} to create a site model
#'   that has both a gamma site model and substitution model
#' @author Richel J.C. Bilderbeek
#' @examples
#'   gamma_site_model <- create_gamma_site_model()
#'   testit::assert(is_gamma_site_model(gamma_site_model))
#' @export
create_gamma_site_model <- function(
  gamma_cat_count = get_default_gamma_cat_count(),
  gamma_shape = get_default_gamma_shape(),
  prop_invariant = get_default_prop_invariant(),
  gamma_shape_prior_distr = create_exp_distr(
    id = 0, # TODO: id must be NA by default
    mean = create_mean_param(
      id = 0,  # TODO: id must be NA by default
      value = "1.0" # string to match XML
    )
  ),
  freq_equilibrium = "estimated"
) {
  list(
    gamma_cat_count = gamma_cat_count,
    gamma_shape = gamma_shape,
    prop_invariant = prop_invariant,
    gamma_shape_prior_distr = gamma_shape_prior_distr
  )
}
