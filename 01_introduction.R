## Load Libraries
## NOTE: As of 8/30/2020, do not allow backports and vctrs to 
## compile from newer sources.

my_packages <- c("tidyverse", "broom", "coefplot", "cowplot", "drat",
                 "gapminder", "GGally", "ggrepel", "ggridges", "gridExtra",
                 "here", "interplot", "margins", "maps", "mapproj",
                 "mapdata", "MASS", "quantreg", "rlang", "scales", "socviz",
                 "survey", "srvyr", "viridis", "viridisLite", "devtools",
                 "rtools","backports","vctrs")


install.packages(my_packages, repos = "http://cran.rstudio.com")

# The following are the packages he habitually loads into memory
# for each chapter
library(gapminder)
library(here)
library(tidyverse)
library(socviz)

# Examine gapminder data set
gapminder


