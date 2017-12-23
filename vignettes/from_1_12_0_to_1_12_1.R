## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(beautier)

## ------------------------------------------------------------------------
is_beast2_input_file_safe <- function() {
  tryCatch(
    return(beautier:::is_beast2_input_file(output_filename)),
    error = function(cond) {}
  )
  TRUE
}

