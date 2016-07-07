#Clear all existing data and close graphics
rm(list=ls())
graphics.off()

library(lattice)
library(nlme)
library(plyr)
library(vegan)

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

sem <- function(x) sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))

#source("nutnet-data-aggregate.q")
#source("nutnet-data-aggregate-raw.q")
comb <- read.csv("comb-by-plot-14-July-2011.csv", header=T, strip.white=T)
comb$region <- as.character(comb$region)
comb$region[comb$region == "Europe"] <- "Eurasia"

cover <- read.csv("full-cover-15-July-2011.csv", header=T, strip.white=T, na.strings=c("NULL", "NA"))
cover$cover <- cover$max_cover
cover$provenance <- cover$local_provenance
cover$lifeform <- cover$local_lifeform
cover$lifespan <- cover$local_lifespan
comb$year <- comb$cover_year
comb$site <- comb$site_code
cover$site <- cover$site_code

#Get cover by functional group
cover <- merge(cover, comb[c("region", "site", "block", "plot", "year", "year_trt")], by=c("site", "block", "plot", "year"))

cover$lifeform <- toupper(as.character(cover$lifeform))
cover$lifespan <- toupper(as.character(cover$lifespan))

cover$lifeform[grep("SHRUB", cover$lifeform)] <- "WOODY"
cover$lifeform[grep("TREE", cover$lifeform)] <- "WOODY"
cover$lifeform[grep("BULB", cover$lifeform)] <- "GRAMINOID"
cover$lifeform[grep("CORM", cover$lifeform)] <- "GRAMINOID"
cover$lifeform[grep("SEDG", cover$lifeform)] <- "GRAMINOID"
cover$lifeform[grep("RUSH", cover$lifeform)] <- "GRAMINOID"
cover$lifeform[grep("CREEP", cover$lifeform)] <- "VINE"
cover$lifeform[grep("FORB", cover$lifeform)] <- "FORB"
cover$lifeform[grep("HERB", cover$lifeform)] <- "FORB"
cover$lifeform[grep("TUBER", cover$lifeform)] <- "FORB"
cover$lifeform[grep("SUCCULENT", cover$lifeform)] <- "FORB"
cover$lifeform[grep("OTHER", cover$lifeform)] <- "UNKNOWN"
cover$lifeform[is.na(cover$lifeform)] <- "UNKNOWN"
cover$lifeform[cover$lifeform == "NA"] <- "UNKNOWN"
cover$lifeform[cover$lifeform == ""] <- "UNKNOWN"
cover$lifeform[cover$lifeform == "?"] <- "UNKNOWN"
cover$lifeform[grep("LICHEN", cover$lifeform)] <- "BRYOPHYTE"
cover$lifeform[grep("MOSS", cover$lifeform)] <- "BRYOPHYTE"

cover$lifeform[grep("Poa", cover$Family)] <- "GRASS"
cover$lifeform[grep("Faba", cover$Family)] <- "LEGUME"

cover$lifespan[is.na(cover$lifespan)] <- "UNKNOWN"
cover$lifespan[grep("PERENNIAL", cover$lifespan)] <- "PERENNIAL"
cover$lifespan[grep("BIENNIAL", cover$lifespan)] <- "BIENNIAL"
cover$lifespan[grep("ANNUAL", cover$lifespan)] <- "ANNUAL"


cover$lifespan[cover$lifespan == "UNKNOWN"] <- "UNKSPAN"
cover$lifeform[cover$lifeform == "UNKNOWN"] <- "UNKFORM"
cover$lifeform[cover$lifeform == "NULL"] <- "UNKFORM"

#Barta Brothers Unknowns are perennial
cover$lifespan[cover$site=="barta.us" & cover$lifespan == "UNKNOWN"] <- "PERENNIAL"

dump(c("cover"), "nutnet-cover-data.Rdata")
