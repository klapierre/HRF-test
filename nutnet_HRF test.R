library(lattice)
library(nlme)
library(vegan)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')


theme_set(theme_bw())
theme_update(axis.title.x=element_text(size=35, vjust=-0.35, margin=margin(t=15)), axis.text.x=element_text(size=30),
             axis.title.y=element_text(size=35, angle=90, vjust=0.5, margin=margin(r=15)), axis.text.y=element_text(size=30),
             plot.title = element_text(size=24, vjust=2),
             panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
             legend.title=element_blank(), legend.text=element_text(size=20))


###bar graph summary statistics function
#barGraphStats(data=, variable="", byFactorNames=c(""))
barGraphStats <- function(data, variable, byFactorNames) {
  count <- length(byFactorNames)
  N <- aggregate(data[[variable]], data[byFactorNames], FUN=length)
  names(N)[1:count] <- byFactorNames
  names(N) <- sub("^x$", "N", names(N))
  mean <- aggregate(data[[variable]], data[byFactorNames], FUN=mean)
  names(mean)[1:count] <- byFactorNames
  names(mean) <- sub("^x$", "mean", names(mean))
  sd <- aggregate(data[[variable]], data[byFactorNames], FUN=sd)
  names(sd)[1:count] <- byFactorNames
  names(sd) <- sub("^x$", "sd", names(sd))
  preSummaryStats <- merge(N, mean, by=byFactorNames)
  finalSummaryStats <- merge(preSummaryStats, sd, by=byFactorNames)
  finalSummaryStats$se <- finalSummaryStats$sd / sqrt(finalSummaryStats$N)
  return(finalSummaryStats)
}  


source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet-basic-data-script.R')
source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet_mean community change.R')

#test on just Konza data
#subset out Konza biomass data
konzaBio <- subset(trtBio, site_code=='konz.us')

#merge biomass and mean change data
konza <- merge(konzaBC, konzaBio, by=c('year', 'plot'))%>%
  group_by(year, trt)%>%
  summarise(live_mass_diff=mean(live_mass_diff), dissimilarity=mean(dissimilarity))

#plot dissimilarity vs biomass change
ggplot(data=subset(konza, trt=='NPK'), aes(x=dissimilarity, y=live_mass_diff, color=year)) +
  geom_point(size=6)

#plot year vs biomass change
konzaBioPlot <- ggplot(data=subset(konza, trt=='NPK'), aes(x=year, y=live_mass_diff, color=dissimilarity)) +
  geom_point(aes(size=dissimilarity)) +
  scale_color_gradient(name='Community\nDissimilarity', low='#330066', high='#FF0000', limits=c(0,1)) +
  scale_size_continuous(range=c(6,20)) +
  scale_x_continuous(breaks=seq(2008,2014,2), limits=c(2008,2015)) +
  scale_y_continuous(breaks=seq(-1,3,1), limits=c(-1,3)) +
  xlab('Year') +
  ylab('Live Biomass Response Ratio') +
  annotate('text', x=2008, y=3, label='(a) Konza Prairie', size=10, hjust='left') +
  theme(legend.title=element_text(size=20, vjust='center', margin=margin(b=50)), legend.position=c(0.8,0.7)) +
  guides(color=F)

#plot year vs dissimilarity
ggplot(data=subset(konza, trt=='NPK'), aes(x=year, y=dissimilarity, color=live_mass_diff)) +
  geom_point(size=6) +
  scale_color_gradient(name='Biomass\nResponse\nRatio', low='#330066', high='#FF0000') +
  xlab('Year') +
  ylab('Bray-Curtis Dissimilarity') +
  theme(legend.title=element_text(size=20, vjust='center'))





#test on just smith prairie data
#subset out smith biomass data
smithBio <- subset(trtBio, site_code=='smith.us')

#merge biomass and mean change data
smith <- merge(smithBC, smithBio, by=c('year', 'plot'))%>%
  group_by(year, trt)%>%
  summarise(live_mass_diff=mean(live_mass_diff), dissimilarity=mean(dissimilarity))

#plot dissimilarity vs biomass change
ggplot(data=subset(smith, trt=='NPK'), aes(x=dissimilarity, y=live_mass_diff, color=year)) +
  geom_point(size=6)

#plot year vs biomass change
smithBioPlot <- ggplot(data=subset(smith, trt=='NPK'), aes(x=year, y=live_mass_diff, color=dissimilarity)) +
  geom_point(aes(size=dissimilarity)) +
  scale_size_continuous(range=c(6,20)) +
  scale_color_gradient(name='Community\nDissimilarity', low='#330066', high='#FF0000', limits=c(0,1)) +
  scale_x_continuous(breaks=seq(2008,2014,2), limits=c(2008,2015)) +
  scale_y_continuous(breaks=seq(-1,3,1), limits=c(-1,3)) +
  xlab('Year') +
  ylab('') +
  annotate('text', x=2008, y=3, label='(b) Smith Prairie', size=10, hjust='left') +
  theme(legend.position='none')

#plot year vs dissimilarity
ggplot(data=subset(smith, trt=='NPK'), aes(x=year, y=dissimilarity, color=live_mass_diff)) +
  geom_point(size=6) +
  scale_color_gradient(name='Biomass\nResponse\nRatio', low='#330066', high='#FF0000') +
  xlab('Year') +
  ylab('Bray-Curtis Dissimilarity') +
  theme(legend.title=element_text(size=20, vjust='center'))






#test on just cedar creek data
#subset out cedar creek biomass data
cdrBio <- subset(trtBio, site_code=='cdcr.us')

#merge biomass and mean change data
cdr <- merge(cdrBC, cdrBio, by=c('year', 'plot'))%>%
  group_by(year, trt)%>%
  summarise(live_mass_diff=mean(live_mass_diff), dissimilarity=mean(dissimilarity))

#plot dissimilarity vs biomass change
ggplot(data=subset(cdr, trt=='NPK'), aes(x=dissimilarity, y=live_mass_diff, color=year)) +
  geom_point(size=6)

#plot year vs biomass change
cdrBioPlot <- ggplot(data=subset(cdr, trt=='NPK'), aes(x=year, y=live_mass_diff, color=dissimilarity)) +
  geom_point(aes(size=dissimilarity)) +
  scale_size_continuous(range=c(6,20)) +
  scale_color_gradient(name='Community\nDissimilarity', low='#330066', high='#FF0000', limits=c(0,1)) +
  scale_y_continuous(breaks=seq(-1,3,1), limits=c(-1,3)) +
  xlab('Year') +
  ylab('') +
  annotate('text', x=2008, y=3, label='(c) Cedar Creek', size=10, hjust='left') +
  theme(legend.position='none')

#plot year vs dissimilarity
ggplot(data=subset(cdr, trt=='NPK'), aes(x=year, y=dissimilarity, color=live_mass_diff)) +
  geom_point(size=6) +
  scale_color_gradient(name='Biomass\nResponse\nRatio', low='#330066', high='#FF0000') +
  xlab('Year') +
  ylab('Bray-Curtis Dissimilarity') +
  theme(legend.title=element_text(size=20, vjust='center'))


pushViewport(viewport(layout=grid.layout(1,3)))
print(konzaBioPlot, vp=viewport(layout.pos.row = 1, layout.pos.col = 1))
print(smithBioPlot, vp=viewport(layout.pos.row = 1, layout.pos.col = 2))
print(cdrBioPlot, vp=viewport(layout.pos.row = 1, layout.pos.col = 3))
#export at 1600x800