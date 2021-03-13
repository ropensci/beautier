# Test if treelog$filename really creates that file
library(beautier)

tracelog_filename <- tempfile()
screenlog_filename <- tempfile()
treelog_filename <- tempfile()

testit::assert(!file.exists(tracelog_filename))
testit::assert(!file.exists(screenlog_filename))
testit::assert(!file.exists(treelog_filename))

inference_model <- beautier::create_inference_model(
  mcmc = create_mcmc(
    chain_length = 3000,
    store_every = 1000,
    tracelog = create_tracelog(
      filename = tracelog_filename
    ),
    screenlog = create_screenlog(
      filename = screenlog_filename
    ),
    treelog = create_treelog(
      filename = treelog_filename
    )

  )
)

beast2_input_filename <- tempfile()

create_beast2_input_file_from_model(
  input_filename = get_fasta_filename(),
  output_filename = beast2_input_filename,
  inference_model = inference_model
)

new_wd <- tempfile()
dir.create(new_wd, recursive = TRUE)
setwd(new_wd)

system2(
  command = "/home/richel/.local/share/beast/bin/beast",
  args = beast2_input_filename
)

testit::assert(file.exists(tracelog_filename))
testit::assert(file.exists(screenlog_filename))
testit::assert(file.exists(treelog_filename))
