#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#'
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @return lines of XML text
#' @author Richèl J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
#' @export
mrca_prior_to_xml_prior_distr <- function(
  inference_model
) {
  # Don't be smart yet
  mrca_prior <- inference_model$mrca_prior
  taxa_names_with_ids <- NULL
  has_non_strict_clock_model <- beautier::get_has_non_strict_clock_model(
    list(inference_model$clock_model)
  )
  testit::assert(beautier::is_mrca_prior(mrca_prior))
  text <- NULL
  if (!has_non_strict_clock_model &&
      !beautier::is_one_na(mrca_prior$mrca_distr) &&
      beautier::is_one_na(inference_model$tipdates_filename)
  ) {
    testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
    text <- c(
      text,
      paste0(
        "<prior id=\"ClockPrior.c:", mrca_prior$alignment_id, "\" ",
        "name=\"distribution\" ",
        "x=\"@clockRate.c:", mrca_prior$alignment_id, "\">"
      )
    )
    text <- c(
      text,
      beautier::indent(
        beautier::distr_to_xml(beautier::create_uniform_distr(
          id = mrca_prior$clock_prior_distr_id)
        )
      )
    )
    text <- c(text, paste0("</prior>"))
  }

  opening_tag <- paste0(
    "<distribution id=\"", mrca_prior$name, ".prior\" ",
    "spec=\"beast.math.distributions.MRCAPrior\" "
  )
  if (mrca_prior$is_monophyletic) {
    opening_tag <- paste0(opening_tag, "monophyletic=\"true\" ")
  }
  testit::assert(!beautier::is_one_na(mrca_prior$alignment_id))
  opening_tag <- paste0(
    opening_tag, "tree=\"@Tree.t:", mrca_prior$alignment_id, "\">"
  )

  text <- c(text, opening_tag)
  text <- c(
    text,
    beautier::indent(
      beautier::mrca_prior_to_xml_taxonset(
        mrca_prior,
        taxa_names_with_ids
      )
    )
  )
  if (beautier::is_distr(mrca_prior$mrca_distr)) {
    text <- c(
      text,
      beautier::indent(beautier::distr_to_xml(mrca_prior$mrca_distr))
    )
  }
  text <- c(text, paste0("</distribution>"))


  text
}
