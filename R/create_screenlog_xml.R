#' Creates the \code{screenlog} section of the \code{logger} section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return the XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_screenlog_xml <- function(
  inference_model = create_inference_model()
) {
  top_line <- "<logger id=\"screenlog\""

  if (inference_model$beauti_options$beast2_version == "2.6") {
    top_line <- paste0(top_line, " spec=\"Logger\"")
  }
  if (nchar(inference_model$mcmc$screenlog$filename) > 0) {
    top_line <- paste0(
      top_line,
      " fileName=\"", inference_model$mcmc$screenlog$filename, "\""
    )
  }
  top_line <- paste0(
    top_line,
    " logEvery=\"", inference_model$mcmc$screenlog$log_every
  )
  if (inference_model$mcmc$screenlog$mode != "autodetect") {
    top_line <- paste0(
      top_line,
      "mode=\"", inference_model$mcmc$screenlog$mode, "\""
    )
  }
  if (inference_model$mcmc$screenlog$sanitise_headers == TRUE) {
    top_line <- paste0(top_line, " sanitiseHeaders=\"true\"")
  }
  if (inference_model$mcmc$screenlog$sort != "none") {
    top_line <- paste0(
      top_line,
      "sort=\"", inference_model$mcmc$screenlog$sort, "\""
    )
  }

  top_line <- paste0(top_line, "\">")

  text <- NULL
  text <- c(text, top_line)
  text <- c(text, "    <log idref=\"posterior\"/>") # nolint this is no absolute path
  if (inference_model$beauti_options$beast2_version %in% c("2.4", "2.5")) {
    text <- c(text, paste0("    <log id=\"ESS.0\" spec=\"util.ESS\" ",
      "arg=\"@posterior\"/>")) # nolint this is no absolute path
  }
  text <- c(text, "    <log idref=\"likelihood\"/>") # nolint this is no absolute path
  text <- c(text, "    <log idref=\"prior\"/>") # nolint this is no absolute path
  text <- c(text, "</logger>")
  text
}
