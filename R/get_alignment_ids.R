#' Get the alignment IDs from one or more files.
#'
#' This is done in the same way as BEAST2 does by default
#' The file extension will be used to determine which
#' type of file is worked on.
#' @param filenames names of the files to be checked
#' @return the IDs extracted from the one or more files
#' @seealso Use \link{get_alignment_ids_from_fasta_filenames} to
#' get the alignment IDs from files known to be FASTA files
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   created <- get_alignment_ids(
#'     get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
#'   )
#'   expected <- c(
#'     get_alignment_id(get_beautier_path("anthus_aco.fas")),
#'     get_alignment_id(get_beautier_path("anthus_nd2.fas"))
#'   )
#'   testit::assert(created == expected)
#' @export
get_alignment_ids <- function(filenames) {
  if (beautier::are_fasta_filenames(filenames)) {
    return(
      beautier::get_alignment_ids_from_fasta_filenames(
        fasta_filenames = filenames
      )
    )
  }
}

#' Get the alignment ID from one or more FASTA filenames.
#'
#' This is done in the same way as BEAST2 does by default.
#' The files are assumed to be FASTA. If this is not the case, there
#' may be any kind of error message when calling this function.
#' @inheritParams default_params_doc
#' @return the IDs from one or more FASTA files
#' @seealso
#' Use \link{get_alignment_ids} to get the alignment IDs from multiple
#' kids of files.
#' Use \link{are_fasta_filenames} to
#' see if the filenames all have a common FASTA filename extension.
#' @author Richèl J.C. Bilderbeek
#' @examples
#'   created <- get_alignment_ids_from_fasta_filenames(
#'     get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
#'   )
#'   expected <- c(
#'     get_alignment_id(get_beautier_path("anthus_aco.fas")),
#'     get_alignment_id(get_beautier_path("anthus_nd2.fas"))
#'   )
#'   testit::assert(created == expected)
#' @export
get_alignment_ids_from_fasta_filenames <- function(fasta_filenames) { #nolint indeed a long function name
  # Do not check if the files are actually FASTA filenames
  ids <- fasta_filenames
  unlist(lapply(ids, beautier::get_alignment_id))
}
