library(lattice)
library(nlme)
library(vegan)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')


theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=40, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=34),
             axis.title.y=element_text(size=40, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=34),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet-basic-data-script.R')
source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet_mean community change.R')

#test on just Konza data
#subset out Konza biomass data
konzaBio <- subset(trtBio, site_code=='konz.us')

#merge biomass and mean change data
konza <- merge(konzaBC, konzaBio, by=c('year', 'plot'))

#plot dissimilarity vs biomass change
ggplot(data=subset(konza, trt=='NPK'), aes(x=dissimilarity, y=live_mass_diff, color=year)) +
  geom_point(size=6)

#plot year vs biomass change
ggplot(data=subset(konza, trt=='NPK'), aes(x=year, y=live_mass_diff, color=dissimilarity)) +
  geom_point(size=6) +
  scale_color_gradient(low='#330066', high='#FF0000') +
  xlab('Year') +
  ylab('Live Biomass Response Ratio')

#plot year vs dissimilarity
ggplot(data=subset(konza, trt=='NPK'), aes(x=year, y=dissimilarity, color=live_mass_diff)) +
  geom_point(size=6) +
  scale_color_gradient(name='Biomass\nResponse\nRatio', low='#330066', high='#FF0000') +
  xlab('Year') +
  ylab('Bray-Curtis Dissimilarity') +
  theme(legend.title=element_text(size=20, vjust='center'))









