library(vegan)
library(plyr)
require(MASS)
require(labdsv)
require(RMySQL)
getwd()
rm(list=ls())

############################################################
############################################################
# Must have established ssh connection to db for this part #
############################################################
############################################################

# create connection to SQL
#m <- dbDriver("MySQL") 	# tells R to use MySQL driver
#con <- dbConnect(m)	# define connection
					# this references .my.conf

# pass query to MySQL db
#rs <- dbSendQuery(con,"call year0_plot_sp_matrix()")

# create dataframe from results: Year 0 matrix
#plot.sp.mat.0 <- fetch(rs, n=-1)
#names(plot.sp.mat.0)[1:20]
#dim(plot.sp.mat.0)
#setwd('~/Dropbox/NutNet_data/data-requests')
#dump(c("plot.sp.mat.0"),file='yr0-sp-matrix.RData')

# pass query to MySQL db
#con <- dbConnect(m)	
#rs <- dbSendQuery(con,"call plot_sp_matrix()")
# create dataframe from results: All observations matrix
#plot.sp.mat.all <- fetch(rs, n=-1)
#names(plot.sp.mat.all)[1:20]
#dim(plot.sp.mat.all)
#setwd('~/Dropbox/NutNet_data/data-requests')
#dump(c("plot.sp.mat.all"),file='allyr-sp-matrix.RData')

###############################
###############################
# Non-SQL version starts here #
###############################
###############################

#
#  Year 0 only
#

# map to directory
setwd('~/Dropbox/NutNet_data/data-requests')

# import 'yr0-sp-matrix' which is data file
source('yr0-sp-matrix.RData')

plot.div.0 <- data.frame(site=plot.sp.mat.0$site_code,
				year=plot.sp.mat.0$year,
				block=plot.sp.mat.0$block,
				plot=plot.sp.mat.0$plot)

# separate identifier data from taxa columns
names(plot.sp.mat.0)[1:10]
# position 5 is first taxon
tax.first <- 5
tax.last <- ncol(plot.sp.mat.0)


# richness is just summary of >0 observations
plot.div.0$rich.vegan <- specnumber(plot.sp.mat.0[,tax.first:tax.last])
# shannon diversity is sum(p[i]*ln(p[i])) where p[i] is proportion of total abundance
plot.div.0$shan <- diversity(plot.sp.mat.0[,tax.first:tax.last])
# Evenness metric is shannon diversity / log(S)
plot.div.0$even <- plot.div.0$shan/log(plot.div.0$rich)

# estimated species richness including chao by site
rich.0 <- specpool(plot.sp.mat.0[,tax.first:tax.last],plot.sp.mat.0$site_code)
rich.0$site <- rownames(rich.0)

# merge observed diversity & specpool stats
plot.div.0 <- merge(plot.div.0,rich.0,by.x=c('site'),by.y=c('site'),all.x=TRUE)

# export as csv for use in SEM
write.csv(plot.div.0,'yr0-site-species-diversity.csv')

# loop to generate species accumulation curves
sites <- unique(plot.div.0$site)
n.sites <- length(sites)
site.spaccum <- list()
for(i in 1:n.sites){
	site.spaccum[[i]] <- specaccum(plot.sp.mat.0[plot.sp.mat.0$site==sites[i],tax.first:tax.last])
}
names(site.spaccum) <- as.character(sites)

# quick plot (prettier methods available)
par(mfrow=c(8,8),cex=0.7,mar=c(1,1,1,1),xaxt='n')
for(i in 1:n.sites){
plot(site.spaccum[[i]],main=sites[i])
}



#
#  All years
#

# map to directory
setwd('~/Dropbox/NutNet_data/data-requests')

# import 'allyr-sp-matrix' which is data file
# NOTE: big file, be patient
source('allyr-sp-matrix.RData')

plot.div.all <- data.frame(site=plot.sp.mat.all$site_code,
				year=plot.sp.mat.all$year,
				block=plot.sp.mat.all$block,
				plot=plot.sp.mat.all$plot)

# separate identifier data from taxa columns
names(plot.sp.mat.all)[1:10]
# position 6 is first taxon
tax.first <- 6
tax.last <- ncol(plot.sp.mat.all)


# richness is just summary of >0 observations
plot.div.all$rich.vegan <- specnumber(plot.sp.mat.all[,tax.first:tax.last])
# shannon diversity is sum(p[i]*ln(p[i])) where p[i] is proportion of total abundance
plot.div.all$shan <- diversity(plot.sp.mat.all[,tax.first:tax.last])
# Evenness metric is shannon diversity / log(S)
plot.div.all$even <- plot.div.all$shan/log(plot.div.all$rich)

# estimated species richness including chao by site
# NOTE for all years, this treats observations across years as independent, which is questionable
rich.all <- specpool(plot.sp.mat.all[,tax.first:tax.last],plot.sp.mat.all$site_code)
rich.all$site <- rownames(rich.all)

# merge observed diversity & specpool stats
plot.div.all <- merge(plot.div.all,rich.all,by.x=c('site'),by.y=c('site'),all.x=TRUE)
plot.div.all[1:20,]
# export as csv for use in SEM
write.csv(plot.div.all,'allyr-site-species-diversity.csv')


# loop to generate species accumulation curves
sites <- unique(plot.div.all$site)
n.sites <- length(sites)
site.spaccum <- list()
for(i in 1:n.sites){
	site.spaccum[[i]] <- specaccum(plot.sp.mat.all[plot.sp.mat.all$site==sites[i],tax.first:tax.last])
}
names(site.spaccum) <- as.character(sites)

# quick plot (prettier methods available)
par(mfrow=c(8,9),cex=0.7,mar=c(1,1,1,1),xaxt='n')
for(i in 1:n.sites){
plot(site.spaccum[[i]],main=sites[i])
}
