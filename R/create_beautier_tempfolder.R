#' Create the default `beautier` temporary folder
#' @return nothing
#' @examples
#' create_beautier_tempfolder()
#'
#' remove_beautier_folder()
#'
#' check_empty_beautier_folder()
#' @author Richèl J.C. Bilderbeek
#' @export
create_beautier_tempfolder <- function() {
  dir.create(
    dirname(get_beautier_tempfilename()),
    showWarnings = FALSE,
    recursive = TRUE
  )
}
