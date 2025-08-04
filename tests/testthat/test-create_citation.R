test_that("create a new style citation", {
  citation <- utils::bibentry(
    bibtype = "article",
    title = "babette: BEAUti 2, BEAST 2 and Tracer for R",
    journal = "Methods in Ecology and Evolution",
    author = person("Richèl JC Bilderbeek and Rampal S Etienne"),
    year = 2008,
    url = "https://doi.org/10.1111/2041-210X.13032"
  )

  # Note, this output is unusable ...
  testthat::expect_output(print(citation, style = "Bibtex"))

  # See an example at
  # https://github.com/tidyverse/ggplot2/blob/main/inst/CITATION

})
