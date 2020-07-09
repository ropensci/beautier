#' Converts a clock model to the \code{branchRateModel} section of the
#' XML as text.
#'
#' This function will be called only if there are no MRCA priors.
#' @inheritParams default_params_doc
#' @return a character vector of XML strings
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #       HERE, where the ID of the distribution is 'likelihood'
#'  #     </distribution>
#'  # </distribution>
#' @export
clock_model_to_xml_lh_distr <- function(
  inference_model,
  clock_model = "deprecated",
  mrca_priors = "deprecated",
  tipdates_filename  = "deprecated"
) {
  if (clock_model != "deprecated") {
    stop("'clock_model' is deprecated, use 'inference_model' instead")
  }
  if (mrca_priors != "deprecated") {
    stop("'mrca_priors' is deprecated, use 'inference_model' instead")
  }
  if (tipdates_filename != "deprecated") {
    stop("'tipdates_filename' is deprecated, use 'inference_model' instead")
  }

  # Do not be smart yet
  clock_model <- inference_model$clock_model
  mrca_priors <- list(inference_model$mrca_prior)
  tipdates_filename <- inference_model$tipdates_filename

  testit::assert(beautier::is_clock_model(clock_model))
  id <- clock_model$id
  testit::assert(beautier::is_id(id))

  text <- NULL
  if (beautier::is_strict_clock_model(clock_model)) {
    if (beautier::is_one_na(tipdates_filename)) {
      text <- c(text, paste0("<branchRateModel id=\"StrictClock.c:",
        id, "\" spec=\"beast.evolution.branchratemodel.StrictClockModel\">"))
      # initialization may happen here
      clock_model$clock_rate_param$id <- id
      text <- c(
        text,
        beautier::indent(
          beautier::parameter_to_xml(clock_model$clock_rate_param)
        )
      )
      text <- c(text, "</branchRateModel>")
    }
    else {
      text <- c(
        text,
        paste0(
          "<branchRateModel id=\"StrictClock.c:", id, "\" ",
          "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
          "clock.rate=\"@clockRate.c:", id, "\"/>" # nolint this is no absolute path
        )
      )
    }
  } else if (beautier::is_rln_clock_model(clock_model)) {
    n_discrete_rates <- clock_model$n_rate_categories
    mparam_id <- clock_model$mparam_id
    line <- paste0("<branchRateModel ",
      "id=\"RelaxedClock.c:", id, "\" ",
      "spec=\"beast.evolution.branchratemodel.UCRelaxedClockModel\" ",
      ifelse(!is_mrca_prior_with_distr(mrca_priors[[1]]),
        "",
        paste0("clock.rate=\"@ucldMean.c:", id, "\" ")
      ),
      ifelse(clock_model$normalize_mean_clock_rate == TRUE,
        "normalize=\"true\" ", ""),
      ifelse(n_discrete_rates != -1,
        paste0("numberOfDiscreteRates=\"", n_discrete_rates, "\" "),
        ""
      ),
      "rateCategories=\"@rateCategories.c:", id, "\" ",
      "tree=\"@Tree.t:", id, "\">"
    )

    text <- c(text, line)
    text <- c(text, paste0("    <LogNormal ",
      "id=\"LogNormalDistributionModel.c:", id, "\" ",
      "S=\"@ucldStdev.c:", id, "\" meanInRealSpace=\"true\" name=\"distr\">"))
    text <- c(text, paste0("        <parameter ",
      "id=\"RealParameter.", mparam_id, "\" ",
      "estimate=\"false\" lower=\"0.0\" name=\"M\" ",
      "upper=\"1.0\">1.0</parameter>"))
    text <- c(text, paste0("    </LogNormal>"))
    if (!is_mrca_prior_with_distr(mrca_priors[[1]])) {
      text <- c(text, paste0("    <parameter ",
        "id=\"ucldMean.c:", id, "\" estimate=\"false\" ",
        "name=\"clock.rate\">", clock_model$mean_clock_rate, "</parameter>"))
    }
    text <- c(text, paste0("</branchRateModel>"))
  }


  testit::assert(is.null(text) || beautier::is_xml(text))
  text
}
