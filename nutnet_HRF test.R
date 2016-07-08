library(lattice)
library(nlme)
library(vegan)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')

source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet-basic-data-script.R')
source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet_mean community change.R')

#test on just Konza data
#subset out Konza biomass data
konzaBio <- subset(trtBio, site_code=='konz.us')

#merge biomass and mean change data
konza <- merge(konzaBC, konzaBio, by=c('year', 'plot'))

#plot
ggplot(data=subset(konza, trt=='NPK'), aes(x=dissimilarity, y=live_mass_diff, color=year)) +
  geom_point()