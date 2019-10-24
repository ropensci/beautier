#' Creates the three logger sections of a BEAST2 XML parameter file
#'
#' The logger section has these elements:
#' \preformatted{
#'  <logger id="tracelog" [...]>
#'      [...]
#'  </logger>
#'  <logger id="screenlog" [...]>
#'      [...]
#'  </logger>
#'  <logger id="treelog.t:[alignment ID]"  [...]>
#'      [...]
#'  </logger>
#' }
#' @inheritParams default_params_doc
#' @seealso
#' Use \link{create_beast2_input_tracelog} to create the XML text
#' of the logger with the \code{tracelog} ID.
#' Use \link{create_beast2_input_screenlog} to create the XML text
#' of the logger with the \code{screenlog} ID.
#' Use \link{create_beast2_input_treelogs} to create the XML text
#' of the loggers with the \code{treelog} ID.
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_loggers <- function(# nolint keep long function name, as it extends the 'create_beast2_input' name
  input_filename,
  inference_model
) {
  testit::assert(length(input_filename) == 1)
  check_inference_model(inference_model)

  tracelog_text <- create_beast2_input_tracelog(
    input_filename = input_filename,
    inference_model = inference_model
  )

  screenlog_text <- beautier::create_beast2_input_screenlog(inference_model)

  treelogs_text <- beautier::create_beast2_input_treelogs(inference_model)


  c(
    beautier::indent(tracelog_text, n_spaces = 4),
    "",
    beautier::indent(screenlog_text, n_spaces = 4),
    beautier::indent(treelogs_text, n_spaces = 4)
  )
}

#' Creates the \code{tracelog} section of the \code{logger} section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_tracelog <- function(# nolint keep long function name, as it extends the 'create_beast2_input' name
  input_filename,
  inference_model
) {
  # Do not be smart yet
  site_models <- list(inference_model$site_model)
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  tipdates_filename <- inference_model$tipdates_filename

  text <- NULL

  text <- c(text, "<log idref=\"posterior\"/>") # nolint this is no absolute path
  text <- c(text, "<log idref=\"likelihood\"/>") # nolint this is no absolute path
  text <- c(text, "<log idref=\"prior\"/>") # nolint this is no absolute path
  text <- c(text, tree_models_to_xml_tracelog(site_models)) # nolint beautier function

  site_models_xml <- site_models_to_xml_tracelog(site_models) # nolint beautier function
  if (!is.null(site_models_xml)) {
    text <- c(text, site_models_xml)
  }

  clock_models_xml <- clock_models_to_xml_tracelog( # nolint beautier function
    clock_models = clock_models,
    mrca_priors = mrca_priors
  )
  if (!is.null(clock_models_xml)) {
    text <- c(text, clock_models_xml)
  }

  text <- c(text, tree_priors_to_xml_tracelog(tree_priors)) # nolint beautier function
  text <- c(
    text,
    mrca_priors_to_xml_tracelog( # nolint beautier function
      clock_models = clock_models,
      mrca_priors = mrca_priors,
      tipdates_filename = tipdates_filename
    )
  )

  # Indent and surround the current text
  text <- beautier::indent(text, n_spaces = 4)

  top_line <- paste0(
    "<logger ",
    "id=\"tracelog\" "
  )
  if (is.na(inference_model$mcmc$tracelog$filename)) {
    # Alignment IDs
    ids <- beautier::get_alignment_id(
      input_filename,
      capitalize_first_char_id =
        inference_model$beauti_options$capitalize_first_char_id
    )
    filename <- utils::head(ids, n = 1)
    top_line <- paste0(
      top_line,
      "fileName=\"", filename, ".log\" "
    )
  } else {
    testit::assert(!is.na(inference_model$mcmc$tracelog$filename))
    top_line <- paste0(
      top_line,
      "fileName=\"", inference_model$mcmc$tracelog$filename, "\" "
    )
  }
  top_line <- paste0(
    top_line,
    "logEvery=\"", inference_model$mcmc$tracelog$log_every, "\" ",
    "model=\"@posterior\""
  )
  if (inference_model$mcmc$tracelog$sanitise_headers == TRUE) {
    top_line <- paste0(
      top_line,
      " sanitiseHeaders=\"true\""
    )
  }
  if (inference_model$mcmc$tracelog$sort != "none") {
    top_line <- paste0(
      top_line,
      " sort=\"", inference_model$mcmc$tracelog$sort, "\""
    )
  }
  top_line <- paste0(top_line, ">")

  text <- c(top_line, text)
  c(text, "</logger>")
}

#' Creates the \code{screenlog} section of the \code{logger} section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @return the XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_screenlog <- function(
  inference_model = create_inference_model()
) {
  top_line <- "<logger id=\"screenlog\""
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
  text <- c(text, paste0("    <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>")) # nolint this is no absolute path
  text <- c(text, "    <log idref=\"likelihood\"/>") # nolint this is no absolute path
  text <- c(text, "    <log idref=\"prior\"/>") # nolint this is no absolute path
  text <- c(text, "</logger>")
  text
}

#' Creates the XML text for the \code{logger} tag with ID \code{treelog}.
#' This section has these elements:
#' \preformatted{
#' <logger id="treelog.t:test_output_0" spec="Logger" fileName="my_treelog.trees" logEvery="345000" mode="tree" sanitiseHeaders="true" sort="smart">
#'     <log id="TreeWithMetaDataLogger.t:test_output_0" spec="beast.evolution.tree.TreeWithMetaDataLogger" tree="@Tree.t:test_output_0"/>
#' </logger>
#' }
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_treelogs <- function(
  inference_model
) {
  top_line <- paste0(
    "<logger id=\"treelog.t:", inference_model$clock_model$id, "\" ",
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
      clock_model_to_xml_treelogger(
        inference_model$clock_model
      ),
      n_spaces = 4
    )
  )
  text <- c(text, "</logger>")
  text
}
