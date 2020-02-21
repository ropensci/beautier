#' Get a function to replace the directory of a filename
#' @param new_dir_name the new directory name
#' @export
get_replace_dir_fun <- function(new_dir_name = "") {
  replace_dir <- function(filename, new_dir_name) {
    stopifnot(length(filename) == 1)
    if (is.na(filename)) return(NA)
    if (filename == "") return("")
    # dirty path may have double seperators,
    # for example '"~//beast2_186c7404208c.xml.state"'
    dirty_path <- file.path(
      new_dir_name,
      basename(filename)
    )
    gsub("//", "/", dirty_path)
  }
  pryr::partial(
    replace_dir,
    new_dir_name = new_dir_name
  )
}
