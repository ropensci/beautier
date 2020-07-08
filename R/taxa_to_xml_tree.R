#' Creates the '\code{tree}' section of a BEAST2 XML parameter file
#'
#' Creates the '\code{tree}' section of a BEAST2 XML parameter file,
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
  inference_model,
  id = "deprecated",
  tipdates_filename = "deprecated"
) {
  if (tipdates_filename != "deprecated") {
    stop("'tipdates_filename' is deprecated, use 'inference_model' instead")
  }
  if (id != "deprecated") {
    stop("'id' is deprecated, use an initialized 'inference_model' instead")
  }

  # Don't be smart yet
  id <- inference_model$tree_prior$id
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(beautier::is_id(id))
  if (beautier::is_one_na(tipdates_filename)) {
    beautier::no_taxa_to_xml_tree(
      id = id,
      inference_model = inference_model
    )
  } else {
    beautier::tipdate_taxa_to_xml_tree(
      id = id,
      tipdates_filename = tipdates_filename
    )
  }
}

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
  id,
  inference_model
) {
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


#' Creates the \code{tree} section
#' (part of the \code{state} section)
#' when there is tip-dating
#' @inheritParams default_params_doc
#' @return the random phylogeny as XML text
#' @author Richèl J.C. Bilderbeek
#' @export
tipdate_taxa_to_xml_tree <- function(
  id,
  tipdates_filename
) {
  testit::assert(beautier::is_id(id))
  testit::assert(!beautier::is_one_na(tipdates_filename))
  trait_set_str <- create_trait_set_string(
    utils::read.table(tipdates_filename, sep = "\t")
  )
  c(
    paste0("<tree id=\"Tree.t:", id, "\" name=\"stateNode\">"),
    paste0("    <trait id=\"dateTrait.t:", id, "\" spec=\"beast.evolution.tree.TraitSet\" traitname=\"date-forward\" value=\"", trait_set_str, "\">"), # nolint indeed a long line
    paste0("        <taxa id=\"TaxonSet.", id, "\" spec=\"TaxonSet\">"),
    paste0("            <alignment idref=\"", id, "\"/>"), # nolint this is no absolute path
    "        </taxa>",
    "    </trait>",
    paste0("    <taxonset idref=\"TaxonSet.", id, "\"/>"), # nolint this is no absolute path
    "</tree>"
  )
}
