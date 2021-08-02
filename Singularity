# The beautier R package

Bootstrap: docker
From: r-base

%post
    sed -i 's/$/ universe/' /etc/apt/sources.list
    apt-get -y update
    apt-get -y install libssl-dev libcurl4-openssl-dev libxml2-dev r-cran-stringi libicu-dev r-cran-rjava
    apt-get -y clean
    Rscript -e 'install.packages(c("remotes", "devtools", "stringi"))'
    Rscript -e 'remotes::install_github("ropensci/beautier")'

%runscript
exec R --vanilla --silent --no-echo "$@"

%test
    Rscript -e 'beautier::get_beautier_folder()'

%help

This container has the R package beautier installed.

To make the container run a script called, e.g. `script.R`, do:

```
cat script.R | ./plinkr.sif
```



```
library(beautier)
# Get an example FASTA file
input_filename <- get_fasta_filename()

# The file created by beautier, a BEAST2 input file
output_filename <- get_beautier_tempfilename()

# Use the default BEAUti settings to create a BEAST2 input file
create_beast2_input_file_from_model(
  input_filename,
  output_filename,
  inference_model = create_inference_model()
)
readLines(output_filename)
file.remove(output_filename)
```

%labels

    AUTHOR Richel J.C. Bilderbeek

    NAME beautier
 
    DESCRIPTION The beautier R package

    USAGE simply run the container

    URL https://github.com/ropensci/beautier

    VERSION 2.6.2
