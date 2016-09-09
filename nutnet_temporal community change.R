library(lattice)
library(nlme)
library(vegan)
library(codyn)
library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')

source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet-cover-data.R')


#test on just Konza data
konzaCover <- subset(nutnetRelCover, site_code=='konz.us')

#create a dataframe with plot numbers
plotList <- data.frame(plot=unique(konzaCover$plot))

#create an empty dataframe to collect dissimilarity values
konzaTempChange <- data.frame(row.names=1)

#for each plot, calculate bray-curtis dissimilarity across all years
for(i in 1:length(plotList$plot)) {
  
  #get site cover data
  dataset=konzaCover[konzaCover$plot==as.character(plotList$plot[i]),]%>%
    #get just first and last year
    filter(year==min(year)|year==max(year))
  
  #calculate disappearance
  disappearance=turnover(df=dataset, time.var='year', species.var='Taxon', abundance.var='rel_cover', replicate.var=NA, metric='disappearance')%>%
    group_by(year)%>%
    summarise(disappearance=mean(disappearance))
  
  #calculate appearance
  appearance=turnover(df=dataset, time.var='year', species.var='Taxon', abundance.var='rel_cover', replicate.var=NA, metric='appearance')%>%
    group_by(year)%>%
    summarise(appearance=mean(appearance))
  
  #merging back with labels to get back plot_mani
  turnover=labels%>%
    left_join(disappearance, by='calendar_year')%>%
    left_join(appearance, by='calendar_year')%>%
    filter(calendar_year==max(calendar_year))%>%
    select(exp_trt, plot_mani, appearance, disappearance)
  
  #pasting variables into the dataframe made for this analysis
  konzaTempChange=rbind(turnover, konzaTempChange)
  
}