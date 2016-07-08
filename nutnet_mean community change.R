library(lattice)
library(nlme)
library(vegan)
library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')

source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet-cover-data.R')

#test on just Konza data
konzaCover <- subset(nutnetRelCover, site_code=='konz.us')%>%
  #transpose
  spread(key=Taxon, value=rel_cover, fill=0)

#create a dataframe with plot numbers
plotList <- data.frame(plot=unique(konzaCover$plot))

#create an empty dataframe to collect dissimilarity values
konzaBC <- data.frame(row.names=1)

#for each plot, calculate bray-curtis dissimilarity across all years
for(i in 1:length(plotList$plot)) {
  
  #get site cover data
  dataset=konzaCover[konzaCover$plot==as.character(plotList$plot[i]),]
  
  #calculate bray-curtis dissimilarity
  BC <- as.data.frame(as.matrix(vegdist(dataset[,6:83], method='bray', diag=F, upper=T)))
  
  #keep just first column of BC dissimilarity matrix
  BC1 <- as.data.frame(BC[,1])
  names(BC1)[names(BC1)=='BC[, 1]'] <- 'dissimilarity'
  
  #gather years of data
  years <- as.data.frame(dataset$year)
  names(years)[names(years)=='dataset$year'] <- 'year'
  years <- years%>%
    mutate(yr0=min(year))%>%
    mutate(BC_years=paste(yr0, year, sep='::'))%>%
    select(BC_years, year)
  
  #bind year labels to dissimilarity data
  dissimilarity <- cbind(BC1, years)%>%
    #add plot label
    mutate(plot=plotList$plot[i])
  
  #paste all data into dataframe made for the analysis
  konzaBC <- rbind(dissimilarity, konzaBC)
  
}



#test on just smith prairie data
smithCover <- subset(nutnetRelCover, site_code=='smith.us')%>%
  #transpose
  spread(key=Taxon, value=rel_cover, fill=0)

#create a dataframe with plot numbers
plotList <- data.frame(plot=unique(smithCover$plot))

#create an empty dataframe to collect dissimilarity values
smithBC <- data.frame(row.names=1)

#for each plot, calculate bray-curtis dissimilarity across all years
for(i in 1:length(plotList$plot)) {
  
  #get site cover data
  dataset=smithCover[smithCover$plot==as.character(plotList$plot[i]),]
  
  #calculate bray-curtis dissimilarity
  BC <- as.data.frame(as.matrix(vegdist(dataset[,6:59], method='bray', diag=F, upper=T)))
  
  #keep just first column of BC dissimilarity matrix
  BC1 <- as.data.frame(BC[,1])
  names(BC1)[names(BC1)=='BC[, 1]'] <- 'dissimilarity'
  
  #gather years of data
  years <- as.data.frame(dataset$year)
  names(years)[names(years)=='dataset$year'] <- 'year'
  years <- years%>%
    mutate(yr0=min(year))%>%
    mutate(BC_years=paste(yr0, year, sep='::'))%>%
    select(BC_years, year)
  
  #bind year labels to dissimilarity data
  dissimilarity <- cbind(BC1, years)%>%
    #add plot label
    mutate(plot=plotList$plot[i])
  
  #paste all data into dataframe made for the analysis
  smithBC <- rbind(dissimilarity, smithBC)
  
}









#test on just cedar creek data
cdrCover <- subset(nutnetRelCover, site_code=='cdcr.us')%>%
  #transpose
  spread(key=Taxon, value=rel_cover, fill=0)

#create a dataframe with plot numbers
plotList <- data.frame(plot=unique(cdrCover$plot))

#create an empty dataframe to collect dissimilarity values
cdrBC <- data.frame(row.names=1)

#for each plot, calculate bray-curtis dissimilarity across all years
for(i in 1:length(plotList$plot)) {
  
  #get site cover data
  dataset=cdrCover[cdrCover$plot==as.character(plotList$plot[i]),]
  
  #calculate bray-curtis dissimilarity
  BC <- as.data.frame(as.matrix(vegdist(dataset[,6:109], method='bray', diag=F, upper=T)))
  
  #keep just first column of BC dissimilarity matrix
  BC1 <- as.data.frame(BC[,1])
  names(BC1)[names(BC1)=='BC[, 1]'] <- 'dissimilarity'
  
  #gather years of data
  years <- as.data.frame(dataset$year)
  names(years)[names(years)=='dataset$year'] <- 'year'
  years <- years%>%
    mutate(yr0=min(year))%>%
    mutate(BC_years=paste(yr0, year, sep='::'))%>%
    select(BC_years, year)
  
  #bind year labels to dissimilarity data
  dissimilarity <- cbind(BC1, years)%>%
    #add plot label
    mutate(plot=plotList$plot[i])
  
  #paste all data into dataframe made for the analysis
  cdrBC <- rbind(dissimilarity, cdrBC)
  
}