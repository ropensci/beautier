library(beautier)

input_fasta_filenames <- beautier:::get_paths(
  c("anthus_aco.fas", "anthus_nd2.fas"))

n_fail <- 0

for (site_model_1 in beautier:::create_site_models()) {
  for (site_model_2 in beautier:::create_site_models()) {
    for (clock_model_1 in beautier:::create_clock_models()) {
      for (clock_model_2 in beautier:::create_clock_models()) {
        for (tree_prior in beautier:::create_tree_priors()) {

          output_xml_filename <- "~/invalid.xml"
          create_beast2_input_file(
            input_fasta_filenames = input_fasta_filenames,
            site_models = list(site_model_1, site_model_2),
            clock_models = list(clock_model_1, clock_model_2),
            tree_priors = list(tree_prior, tree_prior),
            output_xml_filename = output_xml_filename
          )
          is_ok <- beautier:::is_beast2_input_file(output_xml_filename)
          if (!is_ok) {
            print(paste(site_model_1$name, site_model_2$name,
              clock_model_1$name, clock_model_2$name, tree_prior$name))
            beautier::is_beast2_input_file(output_xml_filename,
              verbose = TRUE)
            n_fail <- n_fail + 1
          }
        }
      }
    }
  }
}
quit(status = n_fail, save = "no")
