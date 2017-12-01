#' Puts spaces in between the lines
#' @param lines lines of text
interspace <- function(lines) {
  result <- rep("", time = length(lines) * 2)
  for (i in seq_along(lines)) {
    result[i * 2] <- lines[i]
  }
  result
}
