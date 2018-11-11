#' Creates the two logger sections of a BEAST2 XML parameter file
#' @author Richel J.C. Bilderbeek
#' @inheritParams default_params_doc
#' @noRd
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = list(create_jc69_site_model(id = ids)),
  clock_models = list(create_strict_clock_model(id = ids)),
  tree_priors = list(create_yule_tree_prior(id = ids)),
  mcmc = create_mcmc(),
  mrca_priors = NA
) {
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(are_ids(ids))  # nolint internal function
  testit::assert(are_site_models(site_models)) # nolint internal function
  testit::assert(are_clock_models(clock_models)) # nolint internal function
  testit::assert(are_tree_priors(tree_priors)) # nolint internal function
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  testit::assert(is_mcmc(mcmc)) # nolint internal function

  text <- NULL
  text <- c(
    text,
    indent( # nolint internal function
      create_beast2_input_tracelog( # nolint internal function
        ids = ids,
        site_models = site_models,
        clock_models = clock_models,
        tree_priors = tree_priors,
        mcmc = mcmc,
        mrca_priors = mrca_priors
      ),
      n_spaces = 4
    )
  )

  text <- c(text, "")

  text <- c(text,
    indent( # nolint internal function
      create_beast2_input_screenlog(), # nolint internal function
      n_spaces = 4
    )
  )

  text <- c(text,
    indent( # nolint internal function
      create_beast2_input_treelogs(clock_models), # nolint internal function
      n_spaces = 4
    )
  )

  text
}

#' Creates the tracelog section of the logger section
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
#' @author Richel J.C. Bilderbeek
create_beast2_input_tracelog <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = list(create_jc69_site_model(id = ids)),
  clock_models = list(create_strict_clock_model(id = ids)),
  tree_priors = list(create_yule_tree_prior(id = ids)),
  mcmc = create_mcmc(),
  mrca_priors = NA
) {
  testit::assert(are_ids(ids))  # nolint internal function
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(are_mrca_priors(mrca_priors)) # nolint internal function
  testit::assert(is_mcmc(mcmc)) # nolint internal function

  text <- NULL
  # 1 tracelog
  filename <- utils::head(ids, n = 1)

  text <- c(text, "<log idref=\"posterior\"/>")
  text <- c(text, "<log idref=\"likelihood\"/>")
  text <- c(text, "<log idref=\"prior\"/>")
  text <- c(text, tree_models_to_xml_tracelog(site_models)) # nolint internal function

  site_models_xml <- site_models_to_xml_tracelog(site_models) # nolint internal function
  if (!is.null(site_models_xml)) {
    text <- c(text, site_models_xml)
  }

  clock_models_xml <- clock_models_to_xml_tracelog( # nolint internal function
    clock_models = clock_models,
    mrca_priors = mrca_priors
  )
  if (!is.null(clock_models_xml)) {
    text <- c(text, clock_models_xml)
  }

  text <- c(text, tree_priors_to_xml_tracelog(tree_priors)) # nolint internal function
  text <- c(
    text,
    mrca_priors_to_xml_tracelog( # nolint internal function
      mrca_priors,
      has_non_strict_clock_model = get_has_non_strict_clock_model(clock_models) # nolint internal function
    )
  )

  # Indent and surround the current text
  text <- indent(text, n_spaces = 4) # nolint internal function

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

#' Creates the screenlog section of the logger section
#' of a BEAST2 XML parameter file
#' @return the XML text
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_screenlog <- function() {
  text <- NULL
  text <- c(text, "<logger id=\"screenlog\" logEvery=\"1000\">")
  text <- c(text, "    <log idref=\"posterior\"/>")
  text <- c(text, paste0("    <log id=\"ESS.0\" spec=\"util.ESS\" ",
    "arg=\"@posterior\"/>"))
  text <- c(text, "    <log idref=\"likelihood\"/>")
  text <- c(text, "    <log idref=\"prior\"/>")
  text <- c(text, "</logger>")
  text
}

#' Creates the tracelog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
create_beast2_input_treelogs <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  clock_models
) {
  testit::assert(are_clock_models(clock_models)) # nolint internal function

  text <- NULL
  for (clock_model in clock_models) {

    text <- c(text, "")
    id <- clock_model$id
    text <- c(text, paste0("<logger id=\"treelog.t:", id, "\" ",
      "fileName=\"$(tree).trees\" logEvery=\"1000\" mode=\"tree\">"))
    text <- c(
      text,
      indent(clock_model_to_xml_treelogger(clock_model), n_spaces = 4) # nolint internal function
    )
    text <- c(text, "</logger>")
  }
  text
}
