#' Get a temporary filename
#'
#' Get a temporary filename, similar to \linky{tempfile},
#' except that it always writes to a temporary folder
#' named \link{beautier}.
#' @note this function is added to make sure no temporary
#' cache files are left undeleted
#' @inheritParams tempfile
#' @return name for a temporary file
#' @export
get_beautier_tempfilename <- function(
  pattern = "file",
  fileext = ""
) {
  tempfile(
    pattern = pattern,
    tmpdir = rappdirs::user_cache_dir(appname = "beautier"),
    fileext = fileext
  )
}
