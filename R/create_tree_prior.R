#' Internal function to create a tree prior
#' @note Prefer the use the named functions
#'   \code{\link{create_bd_tree_prior}},
#'   \code{\link{create_cbs_tree_prior}},
#'   \code{\link{create_ccp_tree_prior}}
#'   \code{\link{create_cep_tree_prior}}
#'   and \code{\link{create_yule_tree_prior}}
#'   instead
#' @param name the tree prior name. Can be any name
#'   in \code{\link{get_tree_prior_names}}
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
#' @author Richel J.C. Bilderbeek
#' @examples
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_tree_prior_bd.xml",
#'     tree_priors = create_bd_tree_prior()
#'   )
#'   testit::assert(file.exists("create_tree_prior_bd.xml"))
#'
#'   create_beast2_input_file(
#'     input_filenames = get_beautier_path("test_output_6.fas"),
#'     "create_tree_prior_cbs.xml",
#'     tree_priors = create_cbs_tree_prior()
#'   )
#'   testit::assert(file.exists("create_tree_prior_cbs.xml"))
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_tree_prior_ccp.xml",
#'     tree_priors = create_ccp_tree_prior()
#'   )
#'   testit::assert(file.exists("create_tree_prior_ccp.xml"))
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_tree_prior_cep.xml",
#'     tree_priors = create_cep_tree_prior()
#'   )
#'   testit::assert(file.exists("create_tree_prior_cep.xml"))
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_tree_prior_yule.xml",
#'     tree_priors = create_yule_tree_prior()
#'   )
#'   testit::assert(file.exists("create_tree_prior_yule.xml"))
#' @export
create_tree_prior <- function(
  name,
  id,
  ...
) {
  if (!is_tree_prior_name(name)) {
    tree_priors_as_string <- function() {
      s <- NULL
      for (p in get_tree_prior_names()) {
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
#'   its FASTA filesname using \code{\link{get_id}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   bd_tree_prior <- create_bd_tree_prior()
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_bd_tree_prior.xml",
#'     tree_priors = bd_tree_prior
#'   )
#'   testit::assert(file.exists("create_bd_tree_prior.xml"))
#'
#'   bd_tree_prior_exp <- create_bd_tree_prior(
#'     birth_rate_distr = create_exp_distr()
#'   )
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_bd_tree_prior_exp.xml",
#'     tree_priors = bd_tree_prior_exp
#'   )
#'   testit::assert(file.exists("create_bd_tree_prior_exp.xml"))
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
#'   its FASTA filesname using \code{\link{get_id}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   cbs_tree_prior <- create_cbs_tree_prior()
#'
#'   create_beast2_input_file(
#'     input_filenames = get_beautier_path("test_output_6.fas"),
#'     "create_cbs_tree_prior.xml",
#'     tree_priors = cbs_tree_prior
#'   )
#'   testit::assert(file.exists("create_cbs_tree_prior.xml"))
#' @aliases create_cbs_tree_prior create_tree_prior_cbs
#' @export create_cbs_tree_prior create_tree_prior_cbs
create_cbs_tree_prior <- create_tree_prior_cbs <- function(
  id = NA,
  group_sizes_dimension = 5
  ) {
  cbs_tree_prior <- create_tree_prior(
    name = "coalescent_bayesian_skyline",
    id = id,
    group_sizes_dimension = group_sizes_dimension
  )
  testit::assert(is_cbs_tree_prior(cbs_tree_prior))
  cbs_tree_prior
}

#' Create a Coalescent Constant Population tree prior
#' @param id the ID of the alignment
#' @param pop_size_distr the population distribution,
#'   as created by a \code{\link{create_distr}} function
#' @return a Coalescent Constant Population tree_prior
#' @seealso An alignment ID can be extracted from
#'   its FASTA filesname using \code{\link{get_id}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   ccp_tree_prior <- create_ccp_tree_prior()
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_ccp_tree_prior.xml",
#'     tree_priors = ccp_tree_prior
#'   )
#'   testit::assert(file.exists("create_ccp_tree_prior.xml"))
#' @aliases create_ccp_tree_prior create_tree_prior_ccp
#' @export create_ccp_tree_prior create_tree_prior_ccp
create_ccp_tree_prior <- create_tree_prior_ccp <- function(
  id = NA,
  pop_size_distr = beautier::create_one_div_x_distr()
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
#'   its FASTA filesname using \code{\link{get_id}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   cep_tree_prior <- create_cep_tree_prior()
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_cep_tree_prior.xml",
#'     tree_priors = cep_tree_prior
#'   )
#'   testit::assert(file.exists("create_cep_tree_prior.xml"))
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
#'   its FASTA filesname using \code{\link{get_id}}
#' @usage
#' create_yule_tree_prior(
#'   id = NA,
#'   birth_rate_distr = create_uniform_distr()
#' )
#' @author Richel J.C. Bilderbeek
#' @examples
#'   yule_tree_prior <- create_yule_tree_prior()
#'
#'   create_beast2_input_file(
#'     input_filenames = get_fasta_filename(),
#'     "create_yule_tree_prior.xml",
#'     tree_priors = yule_tree_prior
#'   )
#'   testit::assert(file.exists("create_yule_tree_prior.xml"))
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
