test_that("create a new style citation", {

  # Old style inst/CITATION file:
  #
  #
  #
  # citHeader("To cite reports in publications, please use:")
  #
  # citEntry(
  #  entry = "article",
  #  title="babette: BEAUti 2, BEAST 2 and Tracer for R",
  #  author="Richèl JC Bilderbeek and Rampal S Etienne",
  #  journal="Methods in Ecology and Evolution",
  #  year="2018",
  #  publisher="Wiley Online Library",
  #  url="https://doi.org/10.1111/2041-210X.13032",
  #  textVersion = paste(
  #    "Bilderbeek, Richèl JC, and Etienne, Rampal S.",
  #    "babette: BEAUti 2, BEAST 2 and Tracer for R.",
  #    "Methods in Ecology and Evolution (2018).",
  #    "https://doi.org/10.1111/2041-210X.13032"
  #    )
  # )
  #
  # Create new style, copy-paste the text in `INST/CITATION`
  #
  citation <- utils::bibentry(
    bibtype = "article",
    title = "babette: BEAUti 2, BEAST 2 and Tracer for R",
    journal = "Methods in Ecology and Evolution",
    author = person("Richèl JC Bilderbeek and Rampal S Etienne"),
    year = 2008,
    url = "https://doi.org/10.1111/2041-210X.13032")
  testthat::expect_output(print(citation, style = "Bibtex"))
})
