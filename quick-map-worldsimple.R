#Clear all existing data
rm(list=ls())
#Close graphics windows
graphics.off()

#map nutnet sites
library(maps)
library(maptools)
library(data.table)

#source('~/Dropbox/mysql-cnct.R')
#rs <- dbSendQuery(con,'select site_code, latitude, longitude, experiment_type_id,active from site')
#dat <- fetch(rs,n=-1)
dat <- fread('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data\\sites-lat-long.csv')
dat

#sites<-unique(dat$site_code)
#length(sites)

lat<-as.numeric(dat$latitude)
long<-as.numeric(dat$longitude)
experiment<-dat$experiment
active <- dat$active
#?map
#?wrld_simpl

#par(mfrow=c(1,1), pty="s",las=1)
#map("world", interior=T, col="darkgray" , resolution=1, xlim=c(-125,180), ylim=c(-60,70), lwd=1.5 )
pdf('C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data\\NutNet-map-Sept2016.pdf',height=6,width=10)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-125,180), ylim=c(-60,70), lwd=1.2,border='darkgray')
box()
abline(h=0, col="darkgray", lty=3)
pointcol<-rep('black',length(long))
pointcol[experiment==1]<- "dodgerblue"
pointcol[experiment>1]<- "red"
shps <- rep(16,length(long))
shps[active==0] <- 1
points(long,lat, pch=shps, col=pointcol, cex=2)
map.axes()
legend('bottomleft',horiz=FALSE, pch=16, col=c('dodgerblue','red'), c('Observational', 'Experimental'),cex=1)
legend('bottomright',horiz=FALSE, pch=c(16,1), col='black', c('Active', 'Inactive'),cex=1)
dev.off()
system('open C:\\Users\\Kim\\Dropbox\\working groups\\HRF response - NutNet and CORRE\\NutNet data\\NutNet-map-Sept2016.pdf')

# ggmap
require(ggmap)# bounds is left, bottom, right, top
bounds <- (c(min(long),min(lat),max(long),max(lat)))
mp <-  get_map(location=bounds,maptype='roadmap',source='google',zoom=3)
ggmap(mp)

# the cool thing is then you can annotate them ggplot style
ggmap(mp) + geom_point(aes(x=longitude,y=latitude,col=factor(experiment)),data=dat,cex=5)

# Europe
mp <-  get_map(location='Europe',maptype='roadmap',source='google',zoom=4)
bounds <- c(-10,35,30,60)
mp <-  get_map(location=bounds,maptype='roadmap',source='google',zoom=5)
experiment[experiment==1] <- 'Observational'
experiment[experiment==2] <- 'Factorial Nutrients only'
experiment[experiment==3] <- 'Factorial Nutrient x Fence'
ggmap(mp) + geom_point(aes(x=longitude,y=latitude,col=factor(experiment),shape=factor(active)),data=dat,cex=5) +guides(color=guide_legend(title='experiment type'),shape=guide_legend('active'))
?guides



# Australia
mp <-  get_map(location='Australia',maptype='roadmap',source='osm',zoom=4)
ggmap(mp) + geom_point(aes(x=longitude,y=latitude,col=factor(experiment)),data=dat,cex=5) 

# Nebraska
mp <-  get_map(location='Nebraska',maptype='roadmap',source='google',zoom=6)
ggmap(mp) + geom_point(aes(x=longitude,y=latitude,col=factor(experiment)),data=dat,cex=5)

# UK
mp <-  get_map(location='UK',maptype='roadmap',source='google',zoom=6)
ggmap(mp) + geom_point(aes(x=longitude,y=latitude),data=dat,cex=5)

# South America
mp <- get_map(location='South America',maptype='roadmap',source='google',zoom=3)
ggmap(mp)+ geom_point(aes(x=longitude,y=latitude),data=dat,cex=5)