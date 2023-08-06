#' Internal function to create a tree prior
#' @note Prefer the use the named functions
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}}
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#'   instead
#' @param name the tree prior name. Can be any name
#'   in \code{get_tree_prior_names}
#' @param id the ID of the alignment
#' @param ... specific tree prior parameters
#' @return a tree_prior
#' @seealso See
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}}
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#'   for more examples using those functions
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
#'   check_empty_beautier_folder()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'
#'   bd_tree_prior <- create_bd_tree_prior()
#'   cbs_tree_prior <- create_cbs_tree_prior()
#'   ccp_tree_prior <- create_ccp_tree_prior()
#'   cep_tree_prior <- create_cep_tree_prior()
#'   yule_tree_prior <- create_yule_tree_prior()
#'
#'   # Use any of the above tree priors
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = bd_tree_prior
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#'   check_empty_beautier_folder()
#' }
#' @export
create_tree_prior <- function(
  name,
  id,
  ...
) {
  if (!beautier::is_tree_prior_name(name)) {
    tree_priors_as_string <- function() {
      s <- NULL
      for (p in beautier::get_tree_prior_names()) {
        s <- paste0(s, ", ", p)
      }
      s <- substr(s, start = 3, stop = nchar(s))
      s
    }
    stop(
      "invalid tree prior name, must be one these: ",
      tree_priors_as_string()
    )
  }
  tree_prior <- list(name = name, id = id, ...)
  tree_prior
}

#' Create a Birth-Death tree prior
#' @param id the ID of the alignment
#' @param birth_rate_distr the birth rate distribution,
#'   as created by a \code{\link{create_distr}} function
#' @param death_rate_distr the death rate distribution,
#'   as created by a \code{\link{create_distr}} function
#' @return a Birth-Death tree_prior
#' @usage
#' create_bd_tree_prior(
#'   id = NA,
#'   birth_rate_distr = create_uniform_distr(),
#'   death_rate_distr = create_uniform_distr()
#' )
#' @seealso An alignment ID can be extracted from
#'   its FASTA filename using \code{\link{get_alignment_id}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
#'   bd_tree_prior <- create_bd_tree_prior()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = bd_tree_prior
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#' }
#' @aliases create_bd_tree_prior create_tree_prior_bd
#' @export create_bd_tree_prior create_tree_prior_bd
create_bd_tree_prior <- create_tree_prior_bd <- function(
  id = NA,
  birth_rate_distr = create_uniform_distr(),
  death_rate_distr = create_uniform_distr()
) {
  create_tree_prior(
    name = "birth_death",
    id = id,
    birth_rate_distr = birth_rate_distr,
    death_rate_distr = death_rate_distr
  )
}

#' Create a Coalescent Bayesian Skyline tree prior
#' @inheritParams default_params_doc
#' @return a Coalescent Bayesian Skyline tree_prior
#' @seealso An alignment ID can be extracted from
#'   its FASTA filename using \code{\link{get_alignment_id}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
#'   cbs_tree_prior <- create_cbs_tree_prior()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file(
#'     input_filename = get_beautier_path("test_output_6.fas"),
#'     beast2_input_file,
#'     tree_prior = cbs_tree_prior
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#' }
#' @aliases create_cbs_tree_prior create_tree_prior_cbs
#' @export create_cbs_tree_prior create_tree_prior_cbs
create_cbs_tree_prior <- create_tree_prior_cbs <- function(
  id = NA,
  group_sizes_dimension = 5,
  b_pop_sizes_param = create_b_pop_sizes_param(),
  pop_sizes_scaler_scale_factor = ""
) {
  cbs_tree_prior <- create_tree_prior(
    name = "coalescent_bayesian_skyline",
    id = id,
    group_sizes_dimension = group_sizes_dimension,
    b_pop_sizes_param = b_pop_sizes_param,
    pop_sizes_scaler_scale_factor = pop_sizes_scaler_scale_factor
  )
  testit::assert(beautier::is_cbs_tree_prior(cbs_tree_prior))
  cbs_tree_prior
}

#' Create a Coalescent Constant Population tree prior
#' @param id the ID of the alignment
#' @param pop_size_distr the population distribution,
#'   as created by a \code{\link{create_distr}} function
#' @return a Coalescent Constant Population tree_prior
#' @seealso An alignment ID can be extracted from
#'   its FASTA filename using \code{\link{get_alignment_id}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
#'   ccp_tree_prior <- create_ccp_tree_prior()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = ccp_tree_prior
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#' }
#' @aliases create_ccp_tree_prior create_tree_prior_ccp
#' @export create_ccp_tree_prior create_tree_prior_ccp
create_ccp_tree_prior <- create_tree_prior_ccp <- function(
  id = NA,
  pop_size_distr = beautier::create_one_div_x_distr(
    value = 0.3
  )
) {
  create_tree_prior(
    name = "coalescent_constant_population",
    id = id,
    pop_size_distr = pop_size_distr
  )
}

#' Create a Coalescent Exponential Population tree prior
#' @param id the ID of the alignment
#' @param pop_size_distr the population distribution,
#'   as created by a \code{\link{create_distr}} function
#' @param growth_rate_distr the growth rate distribution,
#'   as created by a \code{\link{create_distr}} function
#' @return a Coalescent Exponential Population tree_prior
#' @seealso An alignment ID can be extracted from
#'   its FASTA filename using \code{\link{get_alignment_id}}
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
#'   cep_tree_prior <- create_cep_tree_prior()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file(
#'     input_filename = get_fasta_filename(),
#'     beast2_input_file,
#'     tree_prior = cep_tree_prior
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#' }
#' @aliases create_cep_tree_prior create_tree_prior_cep
#' @export create_cep_tree_prior create_tree_prior_cep
create_cep_tree_prior <- create_tree_prior_cep <- function(
  id = NA,
  pop_size_distr = create_one_div_x_distr(),
  growth_rate_distr = create_laplace_distr()
) {
  create_tree_prior(
    name = "coalescent_exp_population",
    id = id,
    pop_size_distr = pop_size_distr,
    growth_rate_distr = growth_rate_distr
  )
}

#' Create a Yule tree prior
#' @param id the ID of the alignment
#' @param birth_rate_distr the birth rate distribution,
#'   as created by a \code{\link{create_distr}} function
#' @return a Yule tree_prior
#' @seealso An alignment ID can be extracted from
#'   its FASTA filename using \code{\link{get_alignment_id}}
#' @usage
#' create_yule_tree_prior(
#'   id = NA,
#'   birth_rate_distr = create_uniform_distr()
#' )
#' @author Richèl J.C. Bilderbeek
#' @examples
#' if (is_on_ci()) {
#'
#'   yule_tree_prior <- create_yule_tree_prior()
#'
#'   beast2_input_file <- get_beautier_tempfilename()
#'   create_beast2_input_file(
#'      input_filename = get_fasta_filename(),
#'      beast2_input_file,
#'      tree_prior = yule_tree_prior
#'   )
#'   file.remove(beast2_input_file)
#'
#'   remove_beautier_folder()
#' }
#' @aliases create_yule_tree_prior create_tree_prior_yule
#' @export create_yule_tree_prior create_tree_prior_yule
create_yule_tree_prior <- create_tree_prior_yule <- function(
  id = NA,
  birth_rate_distr = create_uniform_distr()
) {
  create_tree_prior(
    name = "yule",
    id = id,
    birth_rate_distr = birth_rate_distr
  )
}
