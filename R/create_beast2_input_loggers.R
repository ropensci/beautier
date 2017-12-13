#' Creates the two logger sections of a BEAST2 XML parameter file
#' @author Richel J.C. Bilderbeek
#' @inheritParams default_params_doc
create_beast2_input_loggers <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_jc69_site_models(ids = ids),
  clock_models = create_strict_clock_models(ids = ids),
  tree_priors = create_yule_tree_priors(ids = ids)
) {
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))
  testit::assert(beautier::are_ids(ids))
  testit::assert(beautier::are_site_models(site_models))
  testit::assert(beautier::are_clock_models(clock_models))
  testit::assert(beautier::are_tree_priors(tree_priors))

  text <- NULL
  text <- c(text, create_beast2_input_tracelog(
    ids = ids,
    site_models = site_models,
    clock_models = clock_models,
    tree_priors = tree_priors)
  )

  text <- c(text, "")

  text <- c(text,
    indent(
      create_beast2_input_screenlog(),
      n_spaces = 4
    )
  )

  text <- c(text,
    indent(
      create_beast2_input_treelogs(clock_models),
      n_spaces = 4
    )
  )

  text
}


#' Creates the tracelog section of the logger section
#' of a BEAST2 XML parameter file
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
create_beast2_input_tracelog <- function( # nolint keep long function name, as it extends the 'create_beast2_input' name
  ids,
  site_models = create_jc69_site_models(ids = ids),
  clock_models = create_strict_clock_models(ids = ids),
  tree_priors = create_yule_tree_priors(ids = ids)
) {
  testit::assert(beautier::are_ids(ids))
  testit::assert(length(ids) == length(site_models))
  testit::assert(length(ids) == length(clock_models))
  testit::assert(length(ids) == length(tree_priors))

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

  clock_models_xml <- clock_models_to_xml_tracelog(clock_models) # nolint internal function
  if (!is.null(clock_models_xml)) {
    text <- c(text, clock_models_xml)
  }

  text <- c(text, tree_priors_to_xml_tracelog(tree_priors)) # nolint internal function

  text <- indent(text, n_spaces = 4) # nolint internal function

  text <- c(paste0("<logger id=\"tracelog\" fileName=\"",
    filename, ".log\" logEvery=\"1000\" model=\"@posterior\" ",
    "sanitiseHeaders=\"true\" sort=\"smart\">"), text)
  text <- c(text, "</logger>")
  indent(text, n_spaces = 4) # nolint internal function
}

#' Creates the screenlog section of the logger section
#' of a BEAST2 XML parameter file
#' @return the XML text
#' @inheritParams create_beast2_input_loggers
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
#' @inheritParams create_beast2_input_loggers
#' @author Richel J.C. Bilderbeek
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
      indent(clock_model_to_xml_treelogger(clock_model), n_spaces = 4)
    )
    text <- c(text, "</logger>")
  }
  text
}
