library(lattice)
library(nlme)
library(vegan)
library(plyr)
library(dplyr)
library(tidyr)

nutnetCover <- read.csv("full-cover-21-June-2016.csv")%>%
  mutate(lifeform=local_lifeform)

nutnetCover$lifeform[grep("SHRUB", nutnetCover$lifeform)] <- "WOODY"
nutnetCover$lifeform[grep("SHUB", nutnetCover$lifeform)] <- "WOODY"
nutnetCover$lifeform[grep("TREE", nutnetCover$lifeform)] <- "WOODY"
nutnetCover$lifeform[grep("woody", nutnetCover$lifeform)] <- "WOODY"
nutnetCover$lifeform[grep("Woody", nutnetCover$lifeform)] <- "WOODY"
nutnetCover$lifeform[grep("BULB", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("CORM", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("SEDG", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("RUSH", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("GRAM", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("Gram", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("Graminoid", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("Grass", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("grass", nutnetCover$lifeform)] <- "GRAMINOID"
nutnetCover$lifeform[grep("CREEP", nutnetCover$lifeform)] <- "VINE"
nutnetCover$lifeform[grep("FORB", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("Forb", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("HERB", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("Herb", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("herb", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("TUBER", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("SUCCULENT", nutnetCover$lifeform)] <- "FORB"
nutnetCover$lifeform[grep("OTHER", nutnetCover$lifeform)] <- "UNKNOWN"
nutnetCover$lifeform[grep("Vine", nutnetCover$lifeform)] <- "VINE"
nutnetCover$lifeform[is.na(nutnetCover$lifeform)] <- "UNKNOWN"
nutnetCover$lifeform[nutnetCover$lifeform == "NA"] <- "UNKNOWN"
nutnetCover$lifeform[nutnetCover$lifeform == ""] <- "UNKNOWN"
nutnetCover$lifeform[nutnetCover$lifeform == "?"] <- "UNKNOWN"
nutnetCover$lifeform[grep("LICHEN", nutnetCover$lifeform)] <- "BRYOPHYTE"
nutnetCover$lifeform[grep("MOSS", nutnetCover$lifeform)] <- "BRYOPHYTE"
nutnetCover$lifeform[grep("Poa", nutnetCover$Family)] <- "GRASS"
nutnetCover$lifeform[grep("Faba", nutnetCover$Family)] <- "LEGUME"


#pull out pre-treatment data
preTrtCover <- nutnetCover%>%filter(year_trt==0)

#pull out experimental data
trtCover <- nutnetCover%>%filter(year_trt>0)