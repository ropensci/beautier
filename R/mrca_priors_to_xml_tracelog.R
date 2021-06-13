#' Creates the MRCA priors' XML for the tracelog section
#'
#' Creates the MRCA priors' XML for the tracelog section.
#'
#' \code{
#'   <logger id="tracelog" ...>
#'    # Here
#'   </logger>
#' }
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @seealso the complete tracelog section is created
#'   by \code{\link{create_tracelog_xml}}
#' @author Richèl J.C. Bilderbeek
#' @export
mrca_priors_to_xml_tracelog <- function(
  clock_models,
  mrca_priors,
  tipdates_filename = NA
) {
  stop(
    "'mrca_priors_to_xml_tracelog' is deprecated",
    "use 'mrca_prior_to_xml_tracelog' instead"
  )
}
