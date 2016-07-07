library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')

#read in data
nutnetData <- read.csv("comb-by-plot-21-June-2016.csv")

bioData <- nutnetData%>%
  #delete data with no cover
  filter(total_cover!='NULL')%>%
  #drop sites with just 3 plots
  filter(site_code!='ucsc.us', site_code!='elkh.us')%>%
  #get rid of observational only datasets
  filter(experiment_type!='Observational')%>%
  #get rid of exclosure plots
  filter(Exclose==0)%>%
  select(site_code, N, P, K, trt, plot, year_trt, year, rich, site_year_rich, plot_beta, total_mass, live_mass, dead_mass)

#pull out pre-treatment data
preTrtBio <- bioData%>%filter(year_trt==0)

#pull out experimental data
trtBio <- bioData%>%filter(year_trt>0)

#generate site information table
siteInfo <- nutnetData%>%
  select(site_name, site_code, continent, country, region, managed, burned, grazed, anthropogenic, habitat, elevation, latitude, longitude, first_nutrient_year, site_richness, site_native_richness, site_introduced_richness)%>%
  unique()%>%
  #remove sites with observational only datasets
  filter(first_nutrient_year!='NULL')