library(beautier)

create_random <- function(
  input_fasta_filename = beautier:::get_beautier_path("anthus_aco.fas")
) {
  output_xml_filename <- tempfile()
  create_beast2_input_file(
    input_filenames = beautier:::get_beautier_path("anthus_aco.fas"),
    output_filename = output_xml_filename,
    site_models = beautier:::create_rnd_site_model(),
    clock_models = beautier:::create_rnd_clock_model(),
    tree_priors = beautier:::create_rnd_tree_prior()
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
  }
  is_ok
}

seed <- as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 2^31)
#seed <- 0
set.seed(seed)
print(paste("seed:", seed))

status <- 0
# Use one hour
for (i in seq(1, 900)) {
  print(i)
  ok <- create_random()
  if (ok == FALSE) {
    status <- 1
    break
  }
}

print(paste("seed:", seed))

# quit(status = status, save = "no")
