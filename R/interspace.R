#' Puts spaces in between the lines
#' @param lines lines of text
interspace <- function(lines) {

  if (length(lines) == 0) return(lines)

  # number of spaces of non-indented line
  nsni <- count_trailing_spaces(lines[1])

  result <- rep("", time = length(lines) * 2)
  for (i in seq_along(lines)) {
    result[i * 2] <- lines[i]
  }
  result
}
