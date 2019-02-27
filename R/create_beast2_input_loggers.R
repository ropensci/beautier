#' Creates the two logger sections of a BEAST2 XML parameter file
#' @author Richèl J.C. Bilderbeek
#' @inheritParams default_params_doc
#' @noRd
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = list(create_jc69_site_model(id = ids)),
  clock_models = list(create_strict_clock_model(id = ids)),
  tree_priors = list(create_yule_tree_prior(id = ids)),
  mcmc = create_mcmc(),
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(are_ids(ids))  # nolint beautier function
  testit::assert(are_site_models(site_models)) # nolint beautier function
  testit::assert(are_clock_models(clock_models)) # nolint beautier function
  testit::assert(are_tree_priors(tree_priors)) # nolint beautier function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  testit::assert(is_mcmc(mcmc)) # nolint beautier function

  text <- NULL
  text <- c(
    text,
    indent( # nolint beautier function
      create_beast2_input_tracelog( # nolint beautier function
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
    indent( # nolint beautier function
      create_beast2_input_screenlog(), # nolint beautier function
      n_spaces = 4
    )
  )

  text <- c(text,
    indent( # nolint beautier function
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
#'   created <- beautier:::create_beast2_input_tracelog(ids = 1)
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
create_beast2_input_tracelog <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = list(create_jc69_site_model(id = ids)),
  clock_models = list(create_strict_clock_model(id = ids)),
  tree_priors = list(create_yule_tree_prior(id = ids)),
  mcmc = create_mcmc(),
  mrca_priors = NA,
  tipdates_filename = NA
) {
  testit::assert(are_ids(ids))  # nolint beautier function
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(are_mrca_priors(mrca_priors)) # nolint beautier function
  testit::assert(is_mcmc(mcmc)) # nolint beautier function

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
  text <- indent(text, n_spaces = 4) # nolint beautier function

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
  testit::assert(are_clock_models(clock_models)) # nolint beautier function

  text <- NULL
  for (clock_model in clock_models) {

    text <- c(text, "")
    id <- clock_model$id
    text <- c(text, paste0("<logger id=\"treelog.t:", id, "\" ",
      "fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">"))
    text <- c(
      text,
      indent(clock_model_to_xml_treelogger(clock_model), n_spaces = 4) # nolint beautier function
    )
    text <- c(text, "</logger>")
  }
  text
}
