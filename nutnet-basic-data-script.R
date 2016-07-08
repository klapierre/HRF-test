library(plyr)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data')

source('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\HRF-test\\nutnet_weather.R')

###read in data
nutnetData <- read.csv('comb-by-plot-clim-soil-diversity21-Jun-2016.csv')

nutnetBio <- nutnetData%>%
  #delete data with no cover
  filter(total_cover!='NULL')%>%
  #drop sites with just 3 plots
  filter(site_code!='ucsc.us', site_code!='elkh.us')%>%
  #get rid of observational only datasets
  filter(experiment_type!='Observational')%>%
  #get rid of exclosure plots
  filter(Exclose==0)%>%
  #merge with precipitation data
  left_join(nutnetWeather, by=c('site_code', 'year'))%>%
  select(site_code, N, P, K, trt, plot, year_trt, year, rich, site_year_rich, MAT, MAP, RAIN_PET, ppt, plot_beta, total_mass, live_mass, dead_mass)


#examine precipitation relationship - Konza test
summary(precipModel <- glm(live_mass ~ ppt, data=subset(nutnetBio, site_code=='konz.us')))
plot(nutnetBio$live_mass~nutnetBio$ppt)


###calculate difference from pre-treatment year
#pull out pre-treatment data
preTrtBio <- nutnetBio%>%filter(year_trt==0)%>%
  select(site_code, plot, live_mass)
names(preTrtBio)[names(preTrtBio)=="live_mass"] <- "yr0_live_mass"

#pull out experimental data
trtBio <- nutnetBio%>%filter(year_trt>0)%>%
  #merge experimental years with pre-treatment years
  left_join(preTrtBio, by=c('site_code', 'plot'))%>%
  #remove sites without pretreatment data
  filter(!is.na(yr0_live_mass), yr0_live_mass!='NULL')%>%
  #remove plots without treatment year data
  filter(!is.na(live_mass), live_mass!='NULL')%>%
  #calculate change in ANPP from pre-treatment year to each treatment year
  mutate(live_mass=as.numeric(live_mass), yr0_live_mass=as.numeric(yr0_live_mass))%>%
  mutate(live_mass_diff=((live_mass-yr0_live_mass)/yr0_live_mass))



#generate site information table
nutnetSite <- nutnetData%>%
  select(site_name, site_code, continent, country, region, managed, burned, grazed, anthropogenic, habitat, elevation, latitude, longitude, first_nutrient_year, site_richness, site_native_richness, site_introduced_richness)%>%
  unique()%>%
  #remove sites with observational only datasets
  filter(first_nutrient_year!='NULL')