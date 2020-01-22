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
             "ggplot2",
             "dplyr",
             "Rmisc")

diversity <- read_csv("data/diversity/DiversityBiomass.csv")
leafSeedRootShoot <- read_csv("data/diversity/LeafSeedRootShoot.csv")


# Generating site and distance linked total biodiversity
siteSummary <- diversity %>% group_by(Site,Distance) %>% summarise_at(vars(Weight), list(Sum = sum)) 
diversityWithSiteSummary <- inner_join(diversity, siteSummary, by  = c("Site","Distance"))

# Generate unique identifier to easily map down to one entry for each site-distance combination
diversityWithSiteSummary <- within(diversityWithSiteSummary,  GroupId <- paste(Site,Distance , sep=""))
diversityWithSiteSummary <- diversityWithSiteSummary[!duplicated(diversityWithSiteSummary$GroupId),]
diversityWithSiteSummary <- diversityWithSiteSummary%>% select(-one_of("GroupId"))
diversityWithSiteSummary

# Take summary statistics
diversitySEByAliveAndDistance <- summarySE(diversityWithSiteSummary, measurevar = "Sum", groupvars = c("Distance","Alive"))
diversitySEByAliveAndDistance


