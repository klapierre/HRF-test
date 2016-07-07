#Clear all existing data and close graphics
rm(list=ls())
graphics.off()



library(MASS)
library(lattice)
library(nlme)
library(plyr)
library(tree)
library(R.utils)
library(vegan)
library(lme4)
library (simba)

sem <- function(x) sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))

#Read in data
data <- read.csv("comb-by-plot-plus_diversity16-Aug-2011.csv", 
 na.strings=c("NULL", "NA"), 
 header=T, strip.white=T)

#Rename site variable
data$site <- data$site_code
names(data) <- tolower(names(data))

#Delete data with no cover data
data <- data[!is.na(data$rich),]

#Merge and tweak some habitat types
sel <- grep(".nz", data$site, fixed=T)
data$habitat[sel] <-  "OLD FIELD"
sel <- grep("cs.nz", data$site, fixed=T)
data$habitat[sel] <-  "MONTANE GRASSLAND"
sel <- grep("MIXEDGRASS", data$habitat, fixed=T)
data$habitat[sel] <-  "SHORTGRASS PRAIRIE"
sel <- grep("SHORTGRASS", data$habitat, fixed=T)
data$habitat[sel] <-  "SEMIARID GRASSLAND"
sel <- grep("TALLGRASS", data$habitat, fixed=T)
data$habitat[sel] <-  "MESIC GRASSLAND"
sel <- grep("ALPINE", data$habitat, fixed=T)
data$habitat[sel] <-  "ALPINE GRASSLAND"

#Change data frame name
comb <- data

#Check that all sies have plot and site data
comb <- comb[!is.na(comb$plot),]
comb <- comb[!is.na(comb$site),]

#Drop a couple sites with only 3 plots
comb <- comb[comb$site !="ucsc.us",]
comb <- comb[comb$site !="elkh.us",]

#Make absolute value of latitude
comb$latitude <- abs(as.numeric(comb$latitude))

#Calculate temp mean and variance
comb$max.high <- apply(comb[c("jan_high", "jul_high")],1, max, na.rm=T) 
 comb$max.high[comb$max.high == -Inf] <- NA
comb$min.low <- apply(comb[c("jan_low", "jul_low")],1, min, na.rm=T) 
 comb$min.low[comb$min.low == Inf] <- NA
comb$jan.mn <- apply(comb[c("jan_low", "jan_high")],1, mean, na.rm=T) 
comb$jul.mn <- apply(comb[c("jul_low", "jul_high")],1, mean, na.rm=T) 
comb$tmp.rng <- abs(comb$max.high - comb$min.low)
comb$prec.rng <- abs(comb$precip_jan - comb$precip_jul)
comb$mat <- apply(comb[c("jan.mn", "jul.mn")],1, mean, na.rm=T)

comb$continent <- toupper(as.character(comb$continent))
comb$region <- toupper(as.character(comb$region))
comb$habitat <- as.character(comb$habitat)

#Bin some continents
comb$continent[comb$site == "azi.cn"] <-  "EURASIA"
comb$region[comb$site == "azi.cn"] <-  "EURASIA"
comb$continent[comb$continent == "EUROPE"] <-  "EURASIA"
comb$region[comb$region == "EUROPE"] <-  "EURASIA"

#Pull out pre-treatment data
obs.dat <- comb[comb$experiment_type == "Observational" | comb$year_trt == 0,]

#Pull out experimental data
exp.dat <- comb[comb$year_trt > 0,]

exp.dat <- exp.dat[!is.na(exp.dat$year_trt),]

#Get rid of non-standard treatments
###error here###exp.dat <- exp.dat[is.na(exp.dat$treat_other_level) | exp.dat$treat_other_level == "CONTROL" | exp.dat$treat_other_level == "",]

#Make a fertlization variable for TDBU experiment
exp.dat$fert <- NA 
exp.dat$fert[exp.dat$n==1 & exp.dat$p==1 & exp.dat$k==1] <- "FERTILIZED"
exp.dat$fert[exp.dat$n==0 & exp.dat$p==0 & exp.dat$k==0] <- "CONTROL"

#Subset out NPK experiment
#npk.dat <- exp.dat[exp.dat$exclose == 0,]
#npk.dat$trt <- "C"
#npk.dat$trt[npk.dat$n==1 & npk.dat$p==1 & npk.dat$k==1] <- "NPK"
#npk.dat$trt[npk.dat$n==1 & npk.dat$p==0 & npk.dat$k==0] <- "N"
#npk.dat$trt[npk.dat$n==0 & npk.dat$p==1 & npk.dat$k==0] <- "P"
#npk.dat$trt[npk.dat$n==0 & npk.dat$p==0 & npk.dat$k==1] <- "K"
#npk.dat$trt[npk.dat$n==1 & npk.dat$p==1 & npk.dat$k==0] <- "NP"
#npk.dat$trt[npk.dat$n==1 & npk.dat$p==0 & npk.dat$k==1] <- "NK"
#npk.dat$trt[npk.dat$n==0 & npk.dat$p==1 & npk.dat$k==1] <- "PK"

#Subset out TDBU experiment
exc.dat <- exp.dat[!is.na(exp.dat$fert),]
#Find sites with excosures
exc.fnc <- exp.dat[exp.dat$exclose == 1,]
exc.st.lst <- data.frame(unique(exc.fnc$site))
names(exc.st.lst)[1] <- "site"
exc.dat <- merge(exc.dat, exc.st.lst, by="site", all=F)
#Exclude some sites without fences
exc.dat <- exc.dat[exc.dat$site != "temple.us",]
exc.dat <- exc.dat[exc.dat$site != "ukul.za",]

#Make a treatment variable
exc.dat$trt <- "C"
exc.dat$trt[exc.dat$exclose == 1 & exc.dat$fert == "FERTILIZED"] <- "NF"
exc.dat$trt[exc.dat$exclose == 0 & exc.dat$fert == "FERTILIZED"] <- "N"
exc.dat$trt[exc.dat$exclose == 1 & exc.dat$fert == "CONTROL"] <- "F"

#Export data
dump(c("obs.dat", "exp.dat", "npk.dat", "exc.dat"), file="basic-nutnet-data-out.RData")


# CALCULATE EFFECT SIZES####

names(exc.dat)

site<-unique(exc.dat$site_name)
site
length(site)
effect.sizes1<-data.frame()
for(i in 1:length(site)){
	temp<-exc.dat[exc.dat$site_name==site[i]& exc.dat$year_trt==1, ]
	if(dim(temp)[1]>1){
	fertonS1<-((mean(log(temp$rich[temp$trt=="N"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="F"]))))
	herbonS1<-((mean(log(temp$rich[temp$trt=="F"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="N"]))))
	intonS1<-((mean(log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="N"])))+(mean(log(temp$rich[temp$trt=="F"]))))
	fertonE1<-((mean(log(temp$even[temp$trt=="N"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="F"]))))
	herbonE1<-((mean(log(temp$even[temp$trt=="F"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="N"]))))
	intonE1<-((mean(log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="N"])))+(mean(log(temp$even[temp$trt=="F"]))))
	fertonB1<-((mean(log(temp$live_mass[temp$trt=="N"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="F"]))))
	herbonB1<-((mean(log(temp$live_mass[temp$trt=="F"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="N"]))))
	intonB1<-((mean(log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="N"])))+(mean(log(temp$live_mass[temp$trt=="F"]))))
	mean.even1<-mean(temp$even[temp$trt=="C"])
	effect.sizes1<-rbind(effect.sizes1,data.frame(temp[1,1:65],mean.even1, fertonS1, herbonS1, intonS1, fertonE1, herbonE1, intonE1, fertonB1, herbonB1, intonB1))
	rm(temp)
}
}
dim(effect.sizes1)


site<-unique(exc.dat$site_name)
site
length(site)
effect.sizes2<-data.frame()
for(i in 1:length(site)){
	temp<-exc.dat[exc.dat$site_name==site[i]& exc.dat$year_trt==2, ]
	if(dim(temp)[1]>1){
	fertonS2<-((mean(log(temp$rich[temp$trt=="N"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="F"]))))
	herbonS2<-((mean(log(temp$rich[temp$trt=="F"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="N"]))))
	intonS2<-((mean(log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="N"])))+(mean(log(temp$rich[temp$trt=="F"]))))
	fertonE2<-((mean(log(temp$even[temp$trt=="N"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="F"]))))
	herbonE2<-((mean(log(temp$even[temp$trt=="F"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="N"]))))
	intonE2<-((mean(log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="N"])))+(mean(log(temp$even[temp$trt=="F"]))))
	fertonB2<-((mean(log(temp$live_mass[temp$trt=="N"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="F"]))))
	herbonB2<-((mean(log(temp$live_mass[temp$trt=="F"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="N"]))))
	intonB2<-((mean(log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="N"])))+(mean(log(temp$live_mass[temp$trt=="F"]))))
	mean.even2<-mean(temp$even[temp$trt=="C"])
	effect.sizes2<-rbind(effect.sizes2,data.frame(temp[1,1],mean.even2,fertonS2, herbonS2, intonS2, fertonE2, herbonE2, intonE2, fertonB2, herbonB2, intonB2))
	rm(temp)
}
}
dim(effect.sizes2)
names(effect.sizes2)

site<-unique(exc.dat$site_name)
site
length(site)
effect.sizes3<-data.frame()
for(i in 1:length(site)){
	temp<-exc.dat[exc.dat$site_name==site[i]& exc.dat$year_trt==3, ]
	if(dim(temp)[1]>1){
	fertonS3<-((mean(log(temp$rich[temp$trt=="N"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="F"]))))
	herbonS3<-((mean(log(temp$rich[temp$trt=="F"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="N"]))))
	intonS3<-((mean(log(temp$rich[temp$trt=="C"])))+(mean(log(temp$rich[temp$trt=="NF"]))))-(mean((log(temp$rich[temp$trt=="N"])))+(mean(log(temp$rich[temp$trt=="F"]))))
	fertonE3<-((mean(log(temp$even[temp$trt=="N"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="F"]))))
	herbonE3<-((mean(log(temp$even[temp$trt=="F"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="N"]))))
	intonE3<-((mean(log(temp$even[temp$trt=="C"])))+(mean(log(temp$even[temp$trt=="NF"]))))-(mean((log(temp$even[temp$trt=="N"])))+(mean(log(temp$even[temp$trt=="F"]))))
	fertonB3<-((mean(log(temp$live_mass[temp$trt=="N"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="F"]))))
	herbonB3<-((mean(log(temp$live_mass[temp$trt=="F"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="N"]))))
	intonB3<-((mean(log(temp$live_mass[temp$trt=="C"])))+(mean(log(temp$live_mass[temp$trt=="NF"]))))-(mean((log(temp$live_mass[temp$trt=="N"])))+(mean(log(temp$live_mass[temp$trt=="F"]))))
	mean.even3<-mean(temp$even[temp$trt=="C"])
	effect.sizes3<-rbind(effect.sizes3,data.frame(temp[1,1],mean.even3,fertonS3, herbonS3, intonS3, fertonE3, herbonE3, intonE3, fertonB3, herbonB3, intonB3))
	rm(temp)
}
}
dim(effect.sizes3)
names(effect.sizes3)
effect.sizes3$site<-effect.sizes3$temp.1..1.
effect.sizes2$site<-effect.sizes2$temp.1..1.

effects.a <- merge(effect.sizes1, effect.sizes2, by = "site", all = TRUE)
effects.all <- merge(effects.a, effect.sizes3, by = "site", all = TRUE)
effects.all
names(effects.all)
effects.all$mean.even3
effects.all$totalmeaneven<-mean(effects.all$mean.even1, effects.all$mean.even2))



############temporal turnover###############

data <- read.csv("TDBU-betadiv-plot-sp.csv", 
 na.strings=c("NULL", "NA"), 
 header=T, strip.white=T)

dim(data)
names(data)

#first species column = 10, last 1241
#create a unique plot identifier UPI #


data$UPI<-do.call(paste, c(data[c("site_code", "block", "plot")], sep = "_"))
data$UPI

#calcuating temporal turnover

UPI<-unique(data$UPI)
UPI

tto1<-data.frame()
for(i in 1:length(UPI)){   #cycling through each plot
#for(i in 1:5){
	temp<-data[data$UPI==UPI[i], ]
	temp<-temp[order(temp$year_trt),]
	da<-names(temp)[grep("X",names(temp))]   #selecting only species present in plot
	sel<-apply(temp[,da], 2,sum)>0
	templist<-names(temp[,da])[sel]
	if(dim(temp)[1]>1){     #if there are more than one years of plot data
	Jaccard<-1-(sim(temp[,templist], method="jaccard")) #this has been changed from using vegdist 
	Bray<-vegdist(temp[,templist], binary=FALSE)
	meanJacc<-mean(Jaccard) 
	meanBray<-mean(Bray)
	maxrow<-nrow(as.matrix(Bray))
	maxBray<-as.matrix(Bray)[maxrow,1]
	maxJacc<-as.matrix(Jaccard)[maxrow,1]
	TrtYear<-maxrow-1                   ##JPW added this
	tto1<-rbind(tto1,data.frame(temp[1,1:9],meanJacc, meanBray, TrtYear, maxBray, maxJacc)) ###maxrow changed to TrtYear
	rm(temp)
}
}

tto1
nrow(tto1)





############## merge it with effects.all#############
#creating means per site and treatment

tto.mn<-ddply(tto1, .(site_code, N, Exclose), colwise(mean, .(meanJacc, meanBray, TrtYear, maxJacc,maxBray))) ##Maxrow changed from TrtYear
names(tto.mn)

#adding a treatment variable
tto.mn$trt2 <- "C"
tto.mn$trt2[tto.mn$Exclose == 1 & tto.mn$N == "1"] <- "NF"
tto.mn$trt2[tto.mn$Exclose == 0 & tto.mn$N == "1"] <- "N"
tto.mn$trt2[tto.mn$Exclose == 1 & tto.mn$N == "0"] <- "F"

tto.mn

#merging by site
tto.master <- merge(effects.all, tto.mn, by = "site_code", all = FALSE)
dim(tto.master)
tto.master

#### making some plots#############

#### PLOT1 overall ######
 


bwplot(maxBray~trt2,data=tto.mn)
bwplot(meanBray~trt2,tto.mn)
bwplot(maxJacc~trt2,tto.mn)
bwplot(meanJacc~trt2,tto.mn)




#### mixed effect models,maxrow = trtyear because some sites have 2 or 3 ############


is.numeric(tto1$Exclose)
tto1$Exclose<-as.character(tto1$Exclose)
tto1$N<-as.character(tto1$N)

lmer1<-lmer(maxBray~Exclose*N + (1|maxrow)+(1|site_code)+(1|block), data = tto1)
summary(lmer1)

lmer2<-lmer(maxJacc~Exclose*N + (1|maxrow)+(1|site_code)+(1|block), data = tto1)
summary(lmer2)

lmer3<-lmer(meanBray~Exclose*N + (1|maxrow)+(1|site_code)+(1|block), data = tto1)
summary(lmer3)

lmer4<-lmer(meanJacc~Exclose*N + (1|maxrow)+(1|site_code)+(1|block), data = tto1)
summary(lmer4)


############### plotting the relation to effect sizes #### has to be improved
 ##This is where ratios of turnovers would be useful###


############# plotting the impact of the relative species pool ##############

names (tto.master)
xyplot(maxJacc~plot_beta|trt2, tto.master, type=c("r","p"))
xyplot(maxBray~plot_beta|trt2, tto.master, type=c("r","p"))

xyplot(maxJacc~site_richness|trt2, tto.master, type=c("r","p"))
xyplot(maxBray~site_richness|trt2, tto.master, type=c("r","p"))

xyplot(maxJacc~mean.even1|trt2, tto.master, type=c("r","p"))
xyplot(maxBray~mean.even1|trt2, tto.master, type=c("r","p"))


############# plotting correlations with site variables #########


panel.hist <- function(x, ...)
{
     usr <- par("usr"); on.exit(par(usr))
     par(usr = c(usr[1:2], 0, 1.5) )
     h <- hist(x, plot = FALSE)
     breaks <- h$breaks; nB <- length(breaks)
     y <- h$counts; y <- y/max(y)
     rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
     usr <- par("usr"); on.exit(par(usr))
     par(usr = c(0, 1, 0, 1))
     #r <- abs(cor(x, y))
     r <-(cor(x, y, use='complete.obs'))
     txt <- format(c(r, 0.123456789), digits=digits)[1]
     txt <- paste(prefix, txt, sep="")
     if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
     #text(0.5, 0.5, txt, cex = cex * r)
     text(0.5, 0.5, txt, cex = 1.5)
}

pairs(tto.master[c("maxJacc","maxBray", "tmp.rng", "prec.rng", "precipitation", "mat" )],
  upper.panel=panel.cor, lower.panel=panel.smooth,
  diag.panel=panel.hist, cex.labels = 1.1, font.labels=2)

pairs(tto.master[c("maxJacc","maxBray", "latitude", "plot_beta","mean.even1", "elevation","maxrow" )],
  upper.panel=panel.cor, lower.panel=panel.smooth,
  diag.panel=panel.hist, cex.labels = 1.1, font.labels=2)

