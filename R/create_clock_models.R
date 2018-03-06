#' Creates all supported clock models,
#'   which is just a list of the types returned by
#'   \code{\link{create_rln_clock_model}},
#'   and \code{\link{create_strict_clock_model}}
#' @return a list of site_models
#' @author Richel J.C. Bilderbeek
#' @examples
#'  clock_models <- beautier:::create_clock_models()
#'  testit::assert(beautier:::is_rln_clock_model(clock_models[[1]]))
#'  testit::assert(beautier:::is_strict_clock_model(clock_models[[2]]))
create_clock_models <- function() {
  return(
    list(
      create_rln_clock_model(),
      create_strict_clock_model()
    )
  )
}

#' Creates n strict clock_models
#' @param ids the alignment IDs
#' @return a list of strict_clock objects
#' @seealso The alignment IDs can be deduced from the FASTA filenames,
#'   using \code{\link{get_ids}}
#' @examples
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'   clock_models <- create_strict_clock_models(
#'     ids = get_ids(fasta_filenames)
#'   )
#'
#'   create_beast2_input_file(
#'     fasta_filenames,
#'     "create_strict_clock_models.xml",
#'     clock_models = clock_models
#'   )
#'   testit::assert(file.exists("create_strict_clock_models.xml"))
#' @export
create_strict_clock_models <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_strict_clock_model(
      id = ids[i],
      clock_rate_param = create_clock_rate_param(
        id = ids[i]
      )
    )
  }
  ms
}
