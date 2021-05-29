#' Internal function
#'
#' Internal function to creates the '\code{tree}' section
#' of a BEAST2 XML parameter file,
#' which is part of a '\code{state}' section,
#' without being indented.
#'
#' The \code{tree} tag has these elements:
#' \preformatted{
#'    <tree[...]>
#'        <taxonset[...]>
#'        [...]
#'        </taxonset>
#'     </run>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
taxa_to_xml_tree <- function(
  inference_model
) {
  if (beautier::is_one_na(inference_model$tipdates_filename)) {
    beautier::no_taxa_to_xml_tree(
      inference_model = inference_model
    )
  } else {
    beautier::tipdate_taxa_to_xml_tree(
      inference_model = inference_model
    )
  }
}

#' Internal function
#'
#' Creates the '\code{tree}' section of a BEAST2 XML parameter file,
#' which is part of a '\code{state}' section,
#' without being indented,
#' when there is no tip-dating
#'
#' The \code{tree} tag has these elements:
#' \preformatted{
#'    <tree[...]>
#'        <taxonset[...]>
#'        [...]
#'        </taxonset>
#'     </run>
#' }
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
no_taxa_to_xml_tree <- function(
  inference_model,
  id = "deprecated"
) {
  testthat::expect_equal(id, "deprecated")
  id <- inference_model$tree_prior$id
  testit::assert(beautier::is_id(id))
  text <- NULL
  if (inference_model$beauti_options$beast2_version == "2.6") {
    text <- c(
      text,
      paste0(
        "<tree id=\"Tree.t:", id, "\" ",
        "spec=\"beast.evolution.tree.Tree\" ",
        "name=\"stateNode\">"
      )
    )
  } else {
    text <- c(text, paste0("<tree id=\"Tree.t:", id, "\" name=\"stateNode\">"))
  }
  text <- c(text, paste0("    <taxonset id=\"TaxonSet.", id, "\" ",
                         "spec=\"TaxonSet\">"))
  text <- c(text, paste0("        <alignment idref=\"", id, "\"/>")) # nolint this is no absolute path
  text <- c(text, "    </taxonset>")
  text <- c(text, "</tree>")
  if (inference_model$beauti_options$beast2_version == "2.6") {
    # Add spaces in between
    text <- rep(text, each = 2)
    text[2] <- "                "
    text[4] <- "                        "
    text[6] <- "                    "
    text[8] <- "            "
    text <- text[-length(text)]
  }
  text
}

#' Internal function
#'
#' Creates the \code{tree} section
#' (part of the \code{state} section)
#' when there is tip-dating
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
tipdate_taxa_to_xml_tree <- function(
  inference_model,
  id = "deprecated",
  tipdates_filename = "deprecated"
) {
  testthat::expect_equal(id, "deprecated")
  testthat::expect_equal(tipdates_filename, "deprecated")

  # Don't be smart yet
  id <- inference_model$tree_prior$id
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(beautier::is_id(id))
  testit::assert(!beautier::is_one_na(tipdates_filename))

  first_line <- paste0("<tree id=\"Tree.t:", id, "\" ")
  if (inference_model$beauti_options$beast2_version == "2.6") {
    first_line <- paste0(first_line, "spec=\"beast.evolution.tree.Tree\" ")
  }
  first_line <- paste0(first_line, "name=\"stateNode\">")

  c(
    first_line,
    beautier::indent(beautier::tipdate_taxa_to_xml_trait(inference_model)),
    beautier::indent(
      paste0(
        "<taxonset idref=\"TaxonSet.",
        inference_model$site_model$id,
        "\"/>"
      )
    ),
    "</tree>"
  )
}
