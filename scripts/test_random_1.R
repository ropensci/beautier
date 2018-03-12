library(beautier)

create_random <- function(
  input_fasta_filename = beautier:::get_beautier_path("anthus_aco.fas")
) {
  input_filename <- beautier:::get_beautier_path("anthus_aco.fas")
  output_xml_filename <- tempfile()
  site_model <- beautier:::create_rnd_site_model()
  clock_model <- beautier:::create_rnd_clock_model()
  tree_prior <- beautier:::create_rnd_tree_prior()
  mrca_prior <- beautier:::create_rnd_mrca_prior(input_filename)

  create_beast2_input_file(
    input_filenames = input_filename,
    output_filename = output_xml_filename,
    site_models = site_model,
    clock_models = clock_model,
    tree_priors = tree_prior,
    mrca_priors = mrca_prior
  )
  is_ok <- beastier::is_beast2_input_file(output_xml_filename)
  if (!is_ok) {
    print("ERROR")
    file.copy(output_xml_filename, "/home/richel/bad.xml", overwrite = TRUE)
    beastier::is_beast2_input_file(output_xml_filename, verbose = TRUE)
    print("site model:")
    print(site_model)
    print("clock model:")
    print(clock_model)
    print("tree prior:")
    print(tree_prior)
    print("mrca prior:")
    print(mrca_prior)
    print("site model$name:")
    print(site_model$name)
    print("clock model$name:")
    print(clock_model$name)
    print("tree prior$name:")
    print(tree_prior$name)
    print("mrca prior$name:")
    print(mrca_prior$name)
  }
  is_ok
}

seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)
seed <- 0
set.seed(seed)
print(paste("seed:", seed))

status <- 0
# Use one hour
for (i in seq(1, 5)) {
  print(i)
  ok <- create_random()
  if (ok == FALSE) {
    status <- 1
    break
  }
}

print(paste("seed:", seed))

# quit(status = status, save = "no")
