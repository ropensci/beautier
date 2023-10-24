#' Internal function
#'
#' Internal function to creates the '\code{trait}' section
#' of a BEAST2 XML parameter file,
#' which is part of a '\code{tree}' section,
#' without being indented.
#'
#' The \code{tree} tag has these elements:
#' \preformatted{
#' <run[...]>
#'   <state[...]>
#'     <tree[...]>
#'       <trait[...]>
#'       This part
#'       </trait>
#'     </tree>
#'   </run>
#' </state>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
tipdate_taxa_to_xml_trait <- function(inference_model) {
  # Don't be smart yet
  id <- inference_model$tree_prior$id

  # The concatenated taxa
  trait_set_str <- NULL
  if (inference_model$beauti_options$beast2_version != "2.6") {
    trait_set_str <- create_trait_set_string(
      utils::read.table(inference_model$tipdates_filename, sep = "\t")
    )
  }
  first_line <- paste0(
    "<trait id=\"dateTrait.t:", id, "\" ",
    "spec=\"beast.evolution.tree.TraitSet\" "
  )
  if (inference_model$beauti_options$beast2_version != "2.6") {
    first_line <- paste0(
      first_line,
      "traitname=\"date-forward\" "
    )
  } else {
    first_line <- paste0(
      first_line,
      "traitname=\"date\" "
    )
  }
  first_line <- paste0(
    first_line,
    "value=\"", trait_set_str, "\">"
  )

  c(
    first_line,
    paste0("    <taxa id=\"TaxonSet.", id, "\" spec=\"TaxonSet\">"),
    paste0("        <alignment idref=\"", id, "\"/>"), # nolint this is no absolute path
    "    </taxa>",
    "</trait>"
  )
}
