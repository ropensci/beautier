library(beautier)

# All site models, clock models and tree priors

input_fasta_filenames <- beautier:::get_paths(
  c("anthus_aco.fas", "anthus_nd2.fas")
)

# Shared tree priors

for (site_model_1 in beautier::create_site_models()) {
  for (site_model_2 in beautier::create_site_models()) {
    for (clock_model_1 in beautier::create_clock_models()) {
      for (clock_model_2 in beautier::create_clock_models()) {
        for (tree_prior in beautier::create_tree_priors()) {

          lines <- create_beast2_input(
            input_fasta_filenames = input_fasta_filenames,
            site_models = list(site_model_1, site_model_2),
            clock_models = list(clock_model_1, clock_model_2),
            tree_priors = list(tree_prior, tree_prior)
          )
          if (!beautier:::are_beast2_input_lines(lines)) {
            print(
              paste(
                site_model_1$name,
                site_model_2$name,
                clock_model_1$name,
                clock_model_1$name,
                tree_prior$name
              )
            )
          }
          testthat::expect_true(beautier:::are_beast2_input_lines(lines))
        }
      }
    }
  }
}


