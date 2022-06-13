# The beautier R package
# Works, see https://github.com/richelbilderbeek/tidyverse_singularity
Bootstrap: docker
From: rocker/tidyverse

%post
    # From https://github.com/brucemoran/Singularity/blob/8eb44591284ffb29056d234c47bf8b1473637805/shub/bases/recipe.CentOs7-R_3.5.2#L21
    echo 'export LANG=en_US.UTF-8 LANGUAGE=C LC_ALL=C LC_CTYPE=C LC_COLLATE=C  LC_TIME=C LC_MONETARY=C LC_PAPER=C LC_MEASUREMENT=C' >> $SINGULARITY_ENVIRONMENT

    Rscript -e 'install.packages(c("remotes", "devtools"))'
    Rscript -e 'remotes::install_github("ropensci/beautier")'


%runscript
echo "'beautier.sif' running with arguments '$@'"
exec "$@"

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

    VERSION 2.6.5
