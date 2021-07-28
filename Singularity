# The beastier R package, with BEAST2 installed

Bootstrap: docker
From: r-base

%post
    sed -i 's/$/ universe/' /etc/apt/sources.list
    apt-get update
    apt-get -y install libssl-dev libcurl4-openssl-dev libxml2-dev r-cran-stringi libicu-dev r-cran-rjava
    apt-get clean
    Rscript -e 'install.packages(c("remotes", "devtools"))'
    Rscript -e 'remotes::install_github("ropensci/beastier")'
    Rscript -e 'beastier::install_beast2(folder_name = "/opt/beastier")'

%apprun R
exec R "$@"

%apprun Rscript
exec Rscript "$@"

%runscript
exec R "$@"

%test
    Rscript -e 'beastier::beastier_report(folder_name = "/opt/beastier")'

%help

This container has the R package beastier and the tool BEAST2 installed.

When using this container, set `beast2_path` to `/opt/beastier`, for example:

```
library(beastier)
beast2_options <- create_beast2_options(
  input_filename = get_beastier_path("2_4.xml"),
  beast2_path = "/opt/beastier"
)
run_beast2_from_options(beast2_options)
```

%labels

    AUTHOR Richel J.C. Bilderbeek

    NAME beastier
 
    DESCRIPTION The beastier R package, with BEAST2 installed

    USAGE simply run the container

    URL https://github.com/ropensci/beastier

    VERSION 2.4.4
