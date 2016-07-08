library(lattice)
library(nlme)
library(vegan)
library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')


#read in data
nutnetCover <- read.csv('full-cover-21-June-2016.csv')%>%
  mutate(lifeform=local_lifeform)%>%
  #drop cover values for non-living things (these are things like litter, rocks, poop, etc)
  filter(live==1)

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


###calculate relative cover
#calculate total cover within each plot in each year
nutnetTotalCover <- nutnetCover%>%
  group_by(site_code, year, plot)%>%
  summarise(sum_cover=sum(max_cover))

nutnetRelCover <- nutnetCover%>%
  left_join(nutnetTotalCover, by=c('site_code', 'year', 'plot'))%>%
  #calcualte relative cover
  mutate(rel_cover=max_cover/sum_cover)%>%
  select(site_code, year, year_trt, plot, trt, Taxon, rel_cover)


#pull out pre-treatment data
preTrtCover <- nutnetRelCover%>%filter(year_trt==0)

#pull out experimental data
trtCover <- nutnetRelCover%>%filter(year_trt>0)