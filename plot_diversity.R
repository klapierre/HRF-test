# script to import subplot-species matrix using direct connection to MySQL
# to calculate diversity statistics at plot level
# by default takes all years of observation/experiment

library(vegan)
library(plyr)
require(MASS)
require(labdsv)
require(RMySQL)
getwd()
rm(list=ls())

#########################################################
#########################################################
# Must have established ssh connection to db to proceed #
#########################################################
#########################################################

# create connection to SQL
m <- dbDriver("MySQL") 	# tells R to use MySQL driver
con <- dbConnect(m)	# define connection
					# this references .my.conf

# pass query & create dataframe
# pass query and hold results
#rs <- dbSendQuery(con, "select * from pi where PI_name like '%Harpole'")
#df <- fetch(rs)		# create a dataframe from query
#df				# results!

# pass query to MySQL db
rs <- dbSendQuery(con,"call subplot_sp_matrix()")
# create dataframe from results
subplot.sp.mat <- fetch(rs, n=-1)
names(subplot.sp.mat)[1:20]

## as of 1 June 2011: 4150 x 1639 matrix = ~8.1 million cells
#length(which(subplot.sp.mat==0))
# of course all but 100,000 are 0!

# separate identifier data from taxa columns
#names(subplot.sp.mat)[1:10]
# position 6 is first taxon
tax.first <- 6
tax.last <- ncol(subplot.sp.mat)

# make a separate data frame to hold identifiers and summary variables
subplot.div <- data.frame(site=subplot.sp.mat$site_name,				year=subplot.sp.mat$year,
				block=subplot.sp.mat$block,
				plot=subplot.sp.mat$plot,
				subplot=subplot.sp.mat$subplot)
# richness is just summary of >0 observations
subplot.div$rich.vegan <- specnumber(subplot.sp.mat[,tax.first:tax.last])
# shannon diversity is sum(p[i]*ln(p[i])) where p[i] is proportion of total abundance
subplot.div$shan <- diversity(subplot.sp.mat[,tax.first:tax.last])
# Evenness metric is shannon diversity / log(S)
subplot.div$even <- subplot.div$shan/log(subplot.div$rich)

#which(is.na(subplot.div$even))
is.na(subplot.div$even) <- 0

# now have plot-year level metrics
#dim(subplot.div)
#head(subplot.div)
#setwd("/Users/eric/Documents/NutNet/data")
setwd("/Users/elind/Dropbox/NutNet_data")
write.csv(subplot.div,'subplot_diversity_stats.csv')

# roll up to take means
plot.mean.diversity <- ddply(subplot.div, .(site, block, plot, year), colwise(mean, .(rich.vegan, shan, even)))
block.mean.diversity <- ddply(subplot.div, .(site, block, year), colwise(mean, .(rich.vegan, shan, even)))
site.mean.diversity <- ddply(subplot.div, .(site, year), colwise(mean, .(rich.vegan, shan, even)))

# or merge with "comb_by_plot" to include in other analyses
con <- dbConnect(m)
#rs <- dbSendQuery(con, "select * from comb_by_plot")
rs <- dbSendQuery(con, "select * from comb_byplot_clim_soil")
#rs <- dbSendQuery(con, "select * from hobbie_soil_data")
comb <- fetch(rs, n=-1)
comb <- merge(comb, plot.mean.diversity[c("site","block","plot","year","rich.vegan","shan","even")], by.x=c("site_name","block","plot","cover_year"), by.y=c("site","block","plot","year"), all.x=TRUE)
# dim(comb)	# should be same n rows, with three extra vars... 
#comb[which(is.na(comb$shan)),] # sgs messed up plots

# check richness values
#comb[comb$rich.x != comb$rich.y,c("site_name","block","plot","cover_year","INT_rich","NAT_rich","UNK_rich","rich","rich.vegan")]
#length(which(comb$rich.x != comb$rich.y))
#?Sys.time
date.string<-format(Sys.time(),"%d-%b-%Y")
setwd("/Users/elind/Dropbox/NutNet_data/data-requests")
write.table(comb, file = paste("comb-by-plot-clim-soil-diversity",date.string,".csv", sep=""), sep=",", col.names=T,row.names=F,quote=F)
