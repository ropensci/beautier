# Test if treelog$filename really creates that file
library(beautier)

tracelog_filename <- get_beautier_tempfilename()
screenlog_filename <- get_beautier_tempfilename()
treelog_filename <- get_beautier_tempfilename()

testit::assert(!file.exists(tracelog_filename))
testit::assert(!file.exists(screenlog_filename))
testit::assert(!file.exists(treelog_filename))

inference_model <- beautier::create_inference_model(
  mcmc = beautier::create_mcmc(
    chain_length = 3000,
    store_every = 1000,
    tracelog = beautier::create_tracelog(
      filename = tracelog_filename
    ),
    screenlog = beautier::create_screenlog(
      filename = screenlog_filename
    ),
    treelog = beautier::create_treelog(
      filename = treelog_filename
    )

  )
)

beast2_input_filename <- get_beautier_tempfilename()

create_beast2_input_file_from_model(
  input_filename = get_fasta_filename(),
  output_filename = beast2_input_filename,
  inference_model = inference_model
)

new_wd <- get_beautier_tempfilename()
dir.create(new_wd, recursive = TRUE)
setwd(new_wd)

system2(
  command = "/home/richel/.local/share/beast/bin/beast",
  args = beast2_input_filename
)

testit::assert(file.exists(tracelog_filename))
testit::assert(file.exists(screenlog_filename))
testit::assert(file.exists(treelog_filename))
