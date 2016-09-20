library(rmarkdown)

tryCatch(
  rmarkdown::render("vignettes/demo.Rmd", output_file =  "~/demo.html"),
  error = function(msg) { message(msg) }
)

tryCatch(
  system("pandoc ~/demo.html -o demo.pdf"),
  error = function(msg) { message(msg) }
)

warnings()
