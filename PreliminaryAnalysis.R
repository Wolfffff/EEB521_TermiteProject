### EEB521 Termite Project Preliminary Analysis


# 22012020 ----------------------------------------------------------------

if(!require(install.load)){
  install.packages("install.load")
  library(install.load)
}

# WD to context
# https://stackoverflow.com/questions/13672720/r-command-for-setting-working-directory-to-source-file-location-in-rstudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install_load("devtools",
             "tidyverse",
             "ggplot2",
             "dplyr",
             "Rmisc",
             "reshape2",
             "microbiome",             "colorspace",
             "data.table",
             "gridExtra")


diversity <- read_csv("data/diversity/DiversityBiomass.csv")
leafSeedRootShoot <- read_csv("data/diversity/LeafSeedRootShoot.csv")


# Generating site and distance linked total biodiversity
siteAndDistanceSummary <- diversity %>% group_by(Site,Distance) %>% summarise_at(vars(Weight), list(GroupSum = sum)) 
diversityWithSiteSummary <- inner_join(diversity, siteAndDistanceSummary, by  = c("Site","Distance"))

# Generate unique identifier to easily map down to one entry for each site-distance combination
diversityWithSiteSummary <- within(diversityWithSiteSummary,  GroupId <- paste0(Site," ",Distance))
diversityWithSiteSummaryWithoutDuplications <- diversityWithSiteSummary[!duplicated(diversityWithSiteSummary$GroupId),]
#diversityWithSiteSummary <- diversityWithSiteSummary%>% select(-one_of("GroupId"))
diversityWithSiteSummaryWithoutDuplications

# Take summary statistics
diversitySEByAliveAndDistance <- summarySE(diversityWithSiteSummaryWithoutDuplications, measurevar = "GroupSum", groupvars = c("Distance","Alive"))
diversitySEByAliveAndDistance

# 22012020 - Result: DifferenceInTotalBiomassByAliveAndDistance

# Relative Abundance
colours = c( "#A54657",  "#582630", "#F7EE7F", "#4DAA57","#F1A66A","#F26157", "#F9ECCC", "#679289", "#33658A",
             "#F6AE2D","#86BBD8")

diversityWithRelAbund <- diversityWithSiteSummary %>% dplyr::group_by(GroupId)%>% mutate(relAbundByPath = Weight / GroupSum)

ra = ggplot(diversityWithSiteSummary, aes(x = GroupId, fill = Species, y = Weight)) + 
  geom_bar(stat = "identity", colour = "black") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "", y = "Abundance", fill = "OTU") 
ra

aa = ggplot(diversityWithRelAbund, aes(x = GroupId, fill = Species, y = relAbundByPath)) + 
  geom_bar(stat = "identity", colour = "black") + 
  scale_y_continuous(expand = c(0,0)) + 
  labs(x = "", y = "Relative Abundance (%)", fill = "Species") 
aa

# Species Abundance Curve
diversity$Species <- factor(diversity$Species)
color_sa <- setNames(rainbow_hcl(19), levels(diversity$Species))

liveDead <- diversity %>% dplyr::group_by(Alive) %>% dplyr::mutate(GroupSum = sum(Weight)) %>% dplyr::group_by(Alive) %>% dplyr::group_split()

liveDeadDead <- liveDead[[1]]
liveDeadAlive <- liveDead[[2]]

liveDeadDead <- liveDeadDead %>% dplyr::group_by(Species) %>% dplyr::mutate(RelAbund = sum(Weight) / GroupSum)
liveDeadDead <- liveDeadDead[!duplicated(liveDeadDead$Species),]
liveDeadDead$Species <- factor(liveDeadDead$Species, levels = liveDeadDead$Species[order(liveDeadDead$RelAbund,decreasing = T)])
liveDeadDead


liveDeadAlive <- liveDead[[2]]

liveDeadAlive <- liveDeadAlive %>% dplyr::group_by(Species) %>% dplyr::mutate(RelAbund = sum(Weight) / GroupSum)
liveDeadAlive <- liveDeadAlive[!duplicated(liveDeadAlive$Species),]
liveDeadAlive$Species <- factor(liveDeadAlive$Species, levels = liveDeadAlive$Species[order(liveDeadAlive$RelAbund,decreasing = T)])
liveDeadAlive

sa_Dead = ggplot(liveDeadDead, aes(x = Species, fill = Species, y = RelAbund)) + 
  geom_bar(stat = "identity", colour = "black") + 
  ggtitle("Relative Abundance of Species on Dead Termite Mounds") +
  ylim(c(0,.5)) +
  labs(x = "", y = "Relative Abundance (%)", fill = "Species") +
  scale_fill_manual(values = color_sa) +
  theme_minimal() +
  theme(legend.position = "none")
sa_Dead


sa_Alive = ggplot(liveDeadAlive, aes(x = Species, fill = Species, y = RelAbund)) + 
  geom_bar(stat = "identity", colour = "black") + 
  ggtitle("Relative Abundance of Species on Live Termite Mounds") +
  ylim(c(0,.5)) +
  labs(x = "", y = "", fill = "Species") +
  scale_fill_manual(values = color_sa) +
  theme_minimal()
sa_Alive

grid.arrange(sa_Dead,sa_Alive,ncol=2)


# 24012020 ----------------------------------------------------------------

# Analysis of termite mound data by distance and live status

diversitySplitByDistAlive <- diversity %>% dplyr::group_by(Alive,Distance) %>% dplyr::mutate(GroupSum = sum(Weight)) %>% dplyr::group_by(Alive,Distance) %>% dplyr::group_split()

DeadD <- diversitySplitByDistAlive[[2]]
DeadD4 <- diversitySplitByDistAlive[[1]]

LiveD <- diversitySplitByDistAlive[[4]]
LiveD4 <- diversitySplitByDistAlive[[3]]

DeadD <- DeadD %>% dplyr::group_by(Species) %>% dplyr::mutate(RelAbund = sum(Weight) / GroupSum)
DeadD <- DeadD[!duplicated(DeadD$Species),]
DeadD$Species <- factor(DeadD$Species, levels = DeadD$Species[order(DeadD$RelAbund,decreasing = T)])

DeadD4 <- DeadD4 %>% dplyr::group_by(Species) %>% dplyr::mutate(RelAbund = sum(Weight) / GroupSum)
DeadD4 <- DeadD4[!duplicated(DeadD4$Species),]
DeadD4$Species <- factor(DeadD4$Species, levels = DeadD4$Species[order(DeadD4$RelAbund,decreasing = T)])


LiveD <- LiveD %>% dplyr::group_by(Species) %>% dplyr::mutate(RelAbund = sum(Weight) / GroupSum)
LiveD <- LiveD[!duplicated(LiveD$Species),]
LiveD$Species <- factor(LiveD$Species, levels = LiveD$Species[order(LiveD$RelAbund,decreasing = T)])


LiveD4 <- LiveD4 %>% dplyr::group_by(Species) %>% dplyr::mutate(RelAbund = sum(Weight) / GroupSum)
LiveD4 <- LiveD4[!duplicated(LiveD4$Species),]
LiveD4$Species <- factor(LiveD4$Species, levels = LiveD4$Species[order(LiveD4$RelAbund,decreasing = T)])


sa_DeadD = ggplot(DeadD, aes(x = Species, fill = Species, y = RelAbund)) + 
  geom_bar(stat = "identity", colour = "black") + 
  ggtitle("Dead Termite Mounds at Distance D") +
  ylim(c(0,.7)) +
  labs(x = "", y = "", fill = "Species") +
  scale_fill_manual(values = color_sa) +
  theme_minimal() +
  theme(legend.position = "none")

sa_DeadD4 = ggplot(DeadD4, aes(x = Species, fill = Species, y = RelAbund)) + 
  geom_bar(stat = "identity", colour = "black") + 
  ggtitle("Dead Termite Mounds at Distance D4") +
  ylim(c(0,.7)) +
  labs(x = "", y = "", fill = "Species") +
  scale_fill_manual(values = color_sa) +
  theme_minimal() +
  theme(legend.position = "none")


sa_LiveD = ggplot(LiveD, aes(x = Species, fill = Species, y = RelAbund)) + 
  geom_bar(stat = "identity", colour = "black") + 
  ggtitle("Live Termite Mounds at Distance D") +
  ylim(c(0,.7)) +
  labs(x = "", y = "", fill = "Species") +
  scale_fill_manual(values = color_sa) +
  theme_minimal() +
  theme(legend.position = "none")

sa_LiveD4 = ggplot(LiveD4, aes(x = Species, fill = Species, y = RelAbund)) + 
  geom_bar(stat = "identity", colour = "black") + 
  ggtitle("Live Termite Mounds at Distance D4") +
  ylim(c(0,.7)) +
  labs(x = "", y = "", fill = "Species") +
  scale_fill_manual(values = color_sa) +
  theme_minimal() +
  theme(legend.position = "none")


##Result
grid.arrange(sa_DeadD, sa_DeadD4, sa_LiveD,sa_LiveD4,ncol=2,left="Relative Abundance (%)", top="Species Abundances by Live Status and Distance")



