#' Creates the XML text for the `logger` tag with ID `treelog`.
#' This section has these elements:
#' ```
#' <logger id="treelog.t:test_output_0" spec="Logger" fileName="my_treelog.trees" logEvery="345000" mode="tree" sanitiseHeaders="true" sort="smart"> # nolint indeed long
#'     <log id="TreeWithMetaDataLogger.t:test_output_0" spec="beast.evolution.tree.TreeWithMetaDataLogger" tree="@Tree.t:test_output_0"/> # nolint indeed long
#' </logger>
#' ```
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
create_treelog_xml <- function(
  inference_model
) {
  top_line <- paste0(
    "<logger id=\"treelog.t:", inference_model$clock_model$id, "\" "
  )
  if (inference_model$beauti_options$beast2_version == "2.6") {
    top_line <- paste0(top_line, "spec=\"Logger\" ")
  }
  top_line <- paste0(
    top_line,
    "fileName=\"", inference_model$mcmc$treelog$filename, "\" ",
    "logEvery=\"", inference_model$mcmc$treelog$log_every, "\" ",
    "mode=\"", inference_model$mcmc$treelog$mode, "\""
  )
  if (inference_model$mcmc$treelog$sanitise_headers == TRUE) {
    top_line <- paste0(top_line, " sanitiseHeaders=\"true\"")
  }
  if (inference_model$mcmc$treelog$sort != "none") {
    top_line <- paste0(
      top_line,
      "sort=\"", inference_model$mcmc$treelog$sort, "\""
    )
  }
  top_line <- paste0(top_line, ">")

  text <- ""
  text <- c(text, top_line)
  text <- c(
    text,
    beautier::indent(
      beautier::clock_model_to_xml_treelogger(
        inference_model$clock_model
      )
    )
  )
  text <- c(text, "</logger>")
  text
}
