library(beautier)

input_filenames <- beautier:::get_paths(
  c("anthus_aco.fas", "anthus_nd2.fas"))

n_fail <- 0

# Crown ages estimated
for (site_model_1 in beautier:::create_site_models()) {
  for (site_model_2 in beautier:::create_site_models()) {
    for (clock_model_1 in beautier:::create_clock_models()) {
      for (clock_model_2 in beautier:::create_clock_models()) {
        for (tree_prior in beautier:::create_tree_priors()) {
          cat(".")
          output_filename <- "invalid.xml"
          create_beast2_input_file(
            input_filenames = input_filenames,
            site_models = list(site_model_1, site_model_2),
            clock_models = list(clock_model_1, clock_model_2),
            tree_priors = list(tree_prior, tree_prior),
            output_filename = output_filename
          )
          is_ok <- beastier::is_beast2_input_file(output_filename)
          if (!is_ok) {
            print(paste(site_model_1$name, site_model_2$name,
              clock_model_1$name, clock_model_2$name, tree_prior$name))
            beautier::is_beast2_input_file(output_filename,
              verbose = TRUE)
            n_fail <- n_fail + 1
          }
        }
      }
    }
  }
}

# Crown ages all fixed
for (site_model_1 in beautier:::create_site_models()) {
  for (site_model_2 in beautier:::create_site_models()) {
    for (clock_model_1 in beautier:::create_clock_models()) {
      for (clock_model_2 in beautier:::create_clock_models()) {
        for (tree_prior in beautier:::create_tree_priors()) {
          if (runif(n = 1) < 0.9) next
          cat(".")
          output_filename <- "invalid.xml"
          create_beast2_input_file(
            input_filenames = input_filenames,
            site_models = list(site_model_1, site_model_2),
            clock_models = list(clock_model_1, clock_model_2),
            tree_priors = list(tree_prior, tree_prior),
            output_filename = output_filename,
            posterior_crown_age = 15
          )
          is_ok <- beastier::is_beast2_input_file(output_filename)
          if (!is_ok) {
            print(paste(site_model_1$name, site_model_2$name,
              clock_model_1$name, clock_model_2$name, tree_prior$name))
            beautier::is_beast2_input_file(output_filename,
              verbose = TRUE)
            n_fail <- n_fail + 1
          }
        }
      }
    }
  }
}

quit(status = n_fail, save = "no")
