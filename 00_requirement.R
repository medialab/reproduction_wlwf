#===============================================================================
#  File-Name:	00-requirements.R
#  Date:	May 6, 2019
#  Paper:   Who Leads? Who Follows? Measuring Issue Attention and Agenda Setting
#           by Legislators and the Mass Public Using Social Media Data
#  Journal: American Political Science Review
#  Authors: Pablo Barbera, Andreu Casas, Jonathan Nagler, Patrick J. Egan,
#           Richard Bonneau, John Jost, Joshua A. Tucker
#  Purpose: check if required packages for the following R scripts exist and install it if needed
#  Data In:
#           ./data/tweets/text/* & var/lda_results-twokenizer.Rdata &
#           var/lda-ouput/*
#===============================================================================

if (!dir.exists(Sys.getenv("R_LIBS_USER"))) {
dir.create(path = Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
}

# install to local user library path
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, lib = Sys.getenv("R_LIBS_USER"), repos = "https://cran.rstudio.com/")
  }
}

packages <- c(
  "argparse",
  "cvTools",
  "ggdendro",
  "ggplot2",
  "grid",
  "gridExtra",
  "gtable",
  "ggthemes",
  "Matrix",
  "reshape",
  "slam",
  "scales",
  "shiny",
  "tidyverse",
  "tm",
  "topicmodels"
)

invisible(lapply(packages, 
                 install_if_missing)
          )
