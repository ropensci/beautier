#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
#' @param taxa_names_with_ids taxa names that already have received
#'   an ID. Causes the XML to \code{idref} these
#' @param is_first is this the first MRCA prior?
#' @author Richel J.C. Bilderbeek
#' @examples
#'  # <distribution id="posterior" spec="util.CompoundDistribution">
#'  #     <distribution id="prior" spec="util.CompoundDistribution">
#'  #       HERE, where the ID of the distribution is 'prior'
#'  #     </distribution>
#'  #     <distribution id="likelihood" ...>
#'  #     </distribution>
#'  # </distribution>
mrca_prior_to_xml_prior_distr <- function( # nolint internal function
  mrca_prior,
  has_non_strict_clock_model = FALSE,
  taxa_names_with_ids = NULL,
  is_first = TRUE
) {
  testit::assert(is_mrca_prior(mrca_prior))
  text <- NULL
  if (!has_non_strict_clock_model && mrca_prior$is_monophyletic && is_first) {
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
      indent(
        distr_to_xml(create_uniform_distr(
          id = mrca_prior$clock_prior_distr_id)
        ),
        n_spaces = 4
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
  opening_tag <- paste0(
    opening_tag, "tree=\"@Tree.t:", mrca_prior$alignment_id, "\">"
  )

  text <- c(text, opening_tag)
  text <- c(
    text,
    indent(  # nolint internal function
      mrca_prior_to_xml_taxonset(
        mrca_prior,
        taxa_names_with_ids
      ),
      n_spaces = 4
    )
  )
  if (is_distr(mrca_prior$mrca_distr)) {
    text <- c(text, indent(distr_to_xml(mrca_prior$mrca_distr), n_spaces = 4))
  }
  text <- c(text, paste0("</distribution>"))


  text
}
