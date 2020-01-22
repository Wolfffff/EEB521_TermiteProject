### EEB521 Termite Project Preliminary Analysis


# 22012020 ----------------------------------------------------------------

if(!require(install.load)){
  install.packages("install.load")
  library(install.load)
}

# WD to context
# https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install_load("tidyverse",
             "ggplot",
             "dplyr")

diversity <- read_csv("data/diversity/DiversityBiomass.csv")
leafSeedRootShoot <- read_csv("data/diversity/LeafSeedRootShoot.csv")

g1 <- ggplot(diversity, aes(x=Alive, y=Weight)) +
  stat_boxplot(fill = NA)
g1


