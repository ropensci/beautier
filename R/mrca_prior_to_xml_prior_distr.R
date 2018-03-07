#' Creates the distribution section in the prior section of the
#' distribution section of a BEAST2 XML parameter file.
#' These lines start with '<distribution id='
#' @inheritParams default_params_doc
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
  mrca_prior
) {
  testit::assert(is_mrca_prior(mrca_prior))
  text <- NULL
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
      distr_to_xml(create_uniform_distr(id = mrca_prior$clock_prior_distr_id)),
      n_spaces = 4
    )
  )
  text <- c(text, paste0("</prior>"))

  #     <Uniform id="Uniform.0" name="distr" upper="Infinity"/>
  # </prior>
  # <distribution id="all.prior" spec="beast.math.distributions.MRCAPrior" monophyletic="true" tree="@Tree.t:anthus_aco_sub">
  #     <taxonset id="all" spec="TaxonSet">
  #         <taxon id="626029_aco" spec="Taxon"/>
  #         <taxon id="630116_aco" spec="Taxon"/>
  #         <taxon id="630210_aco" spec="Taxon"/>
  #         <taxon id="B25702_aco" spec="Taxon"/>
  #         <taxon id="61430_aco" spec="Taxon"/>
  #     </taxonset>
  #     <Normal id="Normal.0" name="distr">
  #         <parameter id="RealParameter.1" estimate="false" name="mean">0.02</parameter>
  #         <parameter id="RealParameter.2" estimate="false" name="sigma">0.001</parameter>
  #     </Normal>
  # </distribution>

  text
}
