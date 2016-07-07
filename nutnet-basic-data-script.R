#Clear all existing data and close graphics
rm(list=ls())
graphics.off()

#detach("package:lme4")

#library(MASS)
#library(lattice)
#library(nlme)
library(plyr)
#library(tree)
#library(R.utils)

sem <- function(x) sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))

#Read in data
data <- read.csv("comb-by-plot-11-August-2011.csv", 
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

#Bin soem continents
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
exp.dat <- exp.dat[is.na(exp.dat$treat_other_level) | exp.dat$treat_other_level == "CONTROL" | exp.dat$treat_other_level == "",]

#Make a fertlization variable for TDBU experiment
exp.dat$fert <- NA 
exp.dat$fert[exp.dat$n==1 & exp.dat$p==1 & exp.dat$k==1] <- "FERTILIZED"
exp.dat$fert[exp.dat$n==0 & exp.dat$p==0 & exp.dat$k==0] <- "CONTROL"

#Subset out NPK experiment
npk.dat <- exp.dat[exp.dat$exclose == 0,]
npk.dat$trt <- "C"
npk.dat$trt[npk.dat$n==1 & npk.dat$p==1 & npk.dat$k==1] <- "NPK"
npk.dat$trt[npk.dat$n==1 & npk.dat$p==0 & npk.dat$k==0] <- "N"
npk.dat$trt[npk.dat$n==0 & npk.dat$p==1 & npk.dat$k==0] <- "P"
npk.dat$trt[npk.dat$n==0 & npk.dat$p==0 & npk.dat$k==1] <- "K"
npk.dat$trt[npk.dat$n==1 & npk.dat$p==1 & npk.dat$k==0] <- "NP"
npk.dat$trt[npk.dat$n==1 & npk.dat$p==0 & npk.dat$k==1] <- "NK"
npk.dat$trt[npk.dat$n==0 & npk.dat$p==1 & npk.dat$k==1] <- "PK"

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
