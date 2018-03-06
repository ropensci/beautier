#' Creates all supported site models
#'   which is just a list of the types returned by
#'   \code{\link{create_gtr_site_model}},
#'   \code{\link{create_hky_site_model}},
#'   \code{\link{create_jc69_site_model}}
#'   and \code{\link{create_tn93_site_model}}
#' @return a list of site_models
#' @author Richel J.C. Bilderbeek
#' @examples
#'  site_models <- beautier:::create_site_models()
#'  testit::assert(beautier:::is_gtr_site_model(site_models[[1]]))
#'  testit::assert(beautier:::is_hky_site_model(site_models[[2]]))
#'  testit::assert(beautier:::is_jc69_site_model(site_models[[3]]))
#'  testit::assert(beautier:::is_tn93_site_model(site_models[[4]]))
create_site_models <- function() {
  return(
    list(
      beautier::create_gtr_site_model(),
      beautier::create_hky_site_model(),
      beautier::create_jc69_site_model(),
      beautier::create_tn93_site_model()
    )
  )
}

#' Creates a JC69_site_model for each ID
#' @inheritParams default_params_doc
#' @return a list of site_models
#' @seealso The alignment IDs can be deduced from the FASTA filenames,
#'   using \code{\link{get_ids}}
#' @author Richel J.C. Bilderbeek
#' @examples
#'   fasta_filenames <- get_beautier_paths(
#'     c("anthus_aco.fas", "anthus_nd2.fas")
#'   )
#'   site_models <- create_jc69_site_models(c("anthus_aco", "anthus_nd2"))
#'   create_beast2_input_file(
#'     fasta_filenames,
#'     "create_jc69_site_models.xml",
#'     site_models = site_models
#'   )
#'   testit::assert(file.exists("create_jc69_site_models.xml"))
#' @export
create_jc69_site_models <- function(ids) {
  n <- length(ids)
  ms <- list()
  for (i in seq(1, n)) {
    ms[[i]] <- beautier::create_jc69_site_model(id = ids[i])
  }
  ms
}
