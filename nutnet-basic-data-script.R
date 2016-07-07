library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')

#read in data
bioData <- read.csv("comb-by-plot-21-June-2016.csv")%>%
  #delete data with no cover
  filter(total_cover!='NULL')%>%
  #drop sites with just 3 plots
  filter(site_code!='ucsc.us', site_code!='elkh.us')%>%
  #get rid of observational only datasets
  filter(experiment_type!='Observational')%>%
  #get rid of exclosure plots
  filter(Exclose==0)

#pull out pre-treatment data
preTrtBio <- bioData%>%filter(year_trt==0)

#pull out experimental data
trtBio <- bioData%>%filter(year_trt>0)