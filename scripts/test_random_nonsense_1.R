library(beautier)

create_random_anything <- function() {

  anything_index <- sample(x = 1:13, size = 1)

  if (anything_index == 1) {
    beautier:::create_random_site_model()
  } else if (anything_index == 2) {
    beautier:::create_random_clock_model()
  } else if (anything_index == 3) {
    beautier:::create_random_tree_prior()
  } else if (anything_index == 4) {
    beautier:::create_random_gamma_site_model()
  } else if (anything_index == 5) {
    beautier:::create_random_distr()
  } else if (anything_index == 6) {
    beautier:::create_random_freq_equilibrium()
  } else if (anything_index == 7) {
    beautier:::create_random_estimate()
  } else if (anything_index == 8) {
    beautier:::create_random_param()
  } else if (anything_index == 9) {
    "nonsense"
  } else if (anything_index == 10) {
    NA
  } else if (anything_index == 11) {
    NULL
  } else if (anything_index == 12) {
    42
  } else if (anything_index == 13) {
    ape::rcoal(4)
  } else {
    testit::assert(!"Should not get here")
  }
}

create_random <- function(
  input_fasta_filename = beautier:::get_path("anthus_aco.fas")
) {
  output_xml_filename <- tempfile()
  done <- FALSE

  while(done == FALSE) {
    tryCatch(
      {
        create_beast2_input_file(
          input_filenames = beautier:::get_path("anthus_aco.fas"),
          output_filename = output_xml_filename,
          site_models = create_random_anything(),
          clock_models = create_random_anything(),
          tree_priors = create_random_anything()
        )
        done <- TRUE
      },
    error = function(cond) { done <- FALSE }
    )
  }
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
