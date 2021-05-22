#' Get a function that removes the hex string from filenames.
#'
#' The default filenames created by \link{beautier} are temporary files,
#' such as \code{/home/john/.cache/tracelog_82c5888db98.log} (on Linux),
#' where \code{/home/john/.cache} is the location to a temporary folder
#' (on Linux) and \code{tracelog_82c5888db98.log} the filename.
#' The filename ends with a hex string (as is common for temporary files,
#' as \link{tempfile} does so).
#' Because \link{beautier} puts an underscore
#' between the filename description (\code{tracelog}) and the hex
#' string, this function removes both.
#'
#' @author Richèl J.C. Bilderbeek
#' @examples
#'
#' f <- get_remove_hex_fun()
#' # /home/john/beast2.xml.state
#' f("/home/john/beast2_186c7404208c.xml.state")
#'
#' # beast2.xml.state
#' f("beast2_186c7404208c.xml.state")
#'
#' # NA
#' f(NA)
#' @export
get_remove_hex_fun <- function() {
  function(filename) {
    stopifnot(length(filename) == 1)
    if (is.na(filename)) return(NA)
    stringr::str_replace(
      string = filename,
      pattern = "_[0-9a-f]{10,}\\.",
      replacement = "."
    )
  }
}
