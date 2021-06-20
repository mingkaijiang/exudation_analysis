#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(doBy, 
               ggplot2,
               grid,
               cowplot,
               metafor,
               mgcv,
               weights,
               meta,
               igraph,
               tidyverse,
               network)  

#### Sourcing all R files in the modules subdirectory

source_step1 <- dir("function", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z1 in source_step1)source(z1)



