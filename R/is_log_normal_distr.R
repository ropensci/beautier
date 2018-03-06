#' Determine if the object is a valid
#' log-normal distribution,
#' as created by \code{\link{create_log_normal_distr}}
#' @param x an object, to be determined if it is a valid
#'   log-normal distribution
#' @return TRUE if x is a valid log-normal distribution,
#'   FALSE otherwise
#' @seealso use \code{\link{is_distr}} to see if x is any
#'   distribution
#' @author Richel J.C. Bilderbeek
#' @examples
#'   log_normal_distr <- create_log_normal_distr()
#'
#'   input_fasta_filename <- beautier::get_beautier_path("anthus_aco.fas")
#'   create_beast2_input_file(
#'     input_filenames = input_fasta_filename,
#'     "my_beast.xml",
#'     tree_priors = create_yule_tree_prior(
#'       birth_rate_distr = log_normal_distr
#'     )
#'   )
#'   testit::assert(file.exists("my_beast.xml"))
is_log_normal_distr <- function(
  x
) {
  if (!is_distr(x)) return(FALSE)
  if (!"name" %in% names(x)) return(FALSE)
  if (x$name != "log_normal") return(FALSE)
  if (!"m" %in% names(x)) return(FALSE)
  if (!is_m_param(x$m)) return(FALSE)
  if (!"s" %in% names(x)) return(FALSE)
  if (!is_s_param(x$s)) return(FALSE)
  TRUE
}
