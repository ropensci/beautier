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
#'  <logger id="treelog.t:test_output_0"  [...]>
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
  inference_model = create_inference_model()
) {
  check_inference_model(inference_model)
  testit::assert(length(input_filename) == 1)
  # Alignment IDs
  ids <- beautier::get_alignment_id(
    input_filename,
    capitalize_first_char_id =
      inference_model$beauti_options$capitalize_first_char_id
  )

  # Do not be smart yet
  site_models <- list(inference_model$site_model)
  clock_models <- list(inference_model$clock_model)
  tree_priors <- list(inference_model$tree_prior)
  mrca_priors <- list(inference_model$mrca_prior)
  mcmc <- inference_model$mcmc
  tipdates_filename <- inference_model$tipdates_filename

  text <- NULL
  text <- c(
    text,
    beautier::indent(
      create_beast2_input_tracelog(
        ids = ids,
        site_models = site_models,
        clock_models = clock_models,
        tree_priors = tree_priors,
        mcmc = mcmc,
        mrca_priors = mrca_priors,
        tipdates_filename = tipdates_filename
      ),
      n_spaces = 4
    )
  )

  text <- c(text, "")

  text <- c(text,
    beautier::indent(
      create_beast2_input_screenlog(), # nolint beautier function
      n_spaces = 4
    )
  )

  text <- c(text,
    beautier::indent(
      create_beast2_input_treelogs(clock_models), # nolint beautier function
      n_spaces = 4
    )
  )

  text
}

#' Creates the \code{tracelog} section of the \code{logger} section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @examples
#'   created <- create_beast2_input_tracelog(ids = 1)
#'   expected <- c(
#'     paste0(
#'       "<logger id=\"tracelog\" fileName=\"1.log\" ",
#'       "logEvery=\"1000\" model=\"@posterior\" sanitiseHeaders=\"true\" ",
#'       "sort=\"smart\">"
#'     ),
#'     "    <log idref=\"posterior\"/>",
#'     "    <log idref=\"likelihood\"/>",
#'     "    <log idref=\"prior\"/>",
#'     "    <log idref=\"treeLikelihood.1\"/>",
#'     paste0(
#'       "    <log id=\"TreeHeight.t:1\" ",
#'       "spec=\"beast.evolution.tree.TreeHeightLogger\" ",
#'       "tree=\"@Tree.t:1\"/>"
#'     ),
#'     "    <log idref=\"YuleModel.t:1\"/>",
#'     "    <log idref=\"birthRate.t:1\"/>",
#'     "</logger>"
#'    )
#'    testthat::expect_equal(created, expected)
#' @author Richèl J.C. Bilderbeek
#' @export
create_beast2_input_tracelog <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = list(create_jc69_site_model(id = ids)),
  clock_models = list(create_strict_clock_model(id = ids)),
  tree_priors = list(create_yule_tree_prior(id = ids)),
  mcmc = beautier::create_mcmc(),
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(beautier::are_ids(ids))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(beautier::are_mrca_priors(mrca_priors))
  testit::assert(beautier::is_mcmc(mcmc))

  text <- NULL
  # 1 tracelog
  filename <- utils::head(ids, n = 1)

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

  log_every <- mcmc$store_every
  if (log_every == -1) log_every <- 1000 # TODO: calc from mcmc$chain_length

  top_line <- paste0(
    "<logger ",
    "id=\"tracelog\" ",
    "fileName=\"", filename, ".log\" ",
    "logEvery=\"", log_every, "\" ",
    "model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" ",
    "sort=\"smart\"",
    ">"
  )

  text <- c(top_line, text)
  c(text, "</logger>")
}

#' Creates the \code{screenlog} section of the \code{logger} section
#' of a BEAST2 XML parameter file
#' @return the XML text
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
create_beast2_input_screenlog <- function() {
  text <- NULL
  text <- c(text, "<logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "    <log idref=\"posterior\"/>") # nolint this is no absolute path
  text <- c(text, paste0("    <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>")) # nolint this is no absolute path
  text <- c(text, "    <log idref=\"likelihood\"/>") # nolint this is no absolute path
  text <- c(text, "    <log idref=\"prior\"/>") # nolint this is no absolute path
  text <- c(text, "</logger>")
  text
}

#' Creates the \code{tracelog} section of the \code{logger} section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek
create_beast2_input_treelogs <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  clock_models
) {
  testit::assert(beautier::are_clock_models(clock_models))

  text <- NULL
  for (clock_model in clock_models) {

    text <- c(text, "")
    id <- clock_model$id
    text <- c(text, paste0("<logger id=\"treelog.t:", id, "\" ",
      "fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">"))
    text <- c(
      text,
      beautier::indent(clock_model_to_xml_treelogger(clock_model), n_spaces = 4)
    )
    text <- c(text, "</logger>")
  }
  text
}
