#' Internal function called by \link{create_branch_rate_model_xml}
#'
#' It generates the desired XML for some circumstances.
#' Yes, that is a vague description.
#' Would be nice if someone would untangle this :-)
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @export
create_branch_rate_model_stuff_xml <- function(
  inference_model
) {
  # Do not be smart yet
  mrca_prior <- inference_model$mrca_prior
  has_non_strict_clock_model <- beautier::get_has_non_strict_clock_model(
    list(inference_model$clock_model)
  )

  testit::assert(beautier::is_mrca_prior(mrca_prior))
  if (length(mrca_prior) == 1 && beautier::is_one_na(mrca_prior)) {
    return(NULL)
  }
  if (!has_non_strict_clock_model &&
    !beautier::is_one_na(mrca_prior$mrca_distr)
  ) {
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    paste0(
      "<branchRateModel ",
      "id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
      "spec=\"beast.evolution.branchratemodel.StrictClockModel\" ",
      "clock.rate=\"@clockRate.c:", mrca_prior$alignment_id, "\"/>" # nolint this is no absolute path
    )
  } else if (!has_non_strict_clock_model) {
    text <- NULL
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    text <- c(
      text,
      paste0(
        "<branchRateModel id=\"StrictClock.c:", mrca_prior$alignment_id, "\" ",
        "spec=\"beast.evolution.branchratemodel.StrictClockModel\">"
      )
    )
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    text <- c(
      text,
      paste0(
        "    <parameter id=\"clockRate.c:", mrca_prior$alignment_id, "\" ",
        "estimate=\"false\" name=\"clock.rate\">1.0</parameter>"
      )
    )
    text <- c(text, paste0("</branchRateModel>"))
    text
  }
}
