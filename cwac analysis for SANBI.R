# CWAC data analysis for SANBI
# 18 Oct 2019

library(lubridate)

# load functions needed to download CAWC data
source("cwac api data download functions.r")

sites <- get_cwac_sites(province = "northern%20cape") # get northern cape sites

sites[,c("Name","counts","FirstCount","LatestCount","Loc_code")]

cards <- get_cwac_cards(site=sites$Loc_code[sites$Name=="Kamfers Dam"]) # get the card info for Kamfers Dam
cards$startDate <- as.Date(cards$startDate)


species <- c(270,304) # which species do we want to look at? 270 = black-winged stilt; 304 = white-winged tern

recs <- data.frame(matrix(nrow=dim(cards)[1], ncol = length(species)))
colnames(recs) <- paste("X",species, sep='')

for (i in 1:dim(cards)[1]) {
 cd <- get_cwac_records(card=cards$Card[i])
 for (j in 1:length(species)) {
   x <- as.numeric(as.character(cd$count[which(cd$spp== species[j])]))
   if (length(x)==1) recs[i,j] <- x
 }
}

recs[is.na(recs)] <- 0  # turning missing values into 0s, assuming that all waterbird species were always counted if they were there (and seen...)
recs$startDate <- cards$startDate
recs$year <- year(recs$startDate)
recs$season <- cards$Season

recs <- recs[order(recs$startDate),]  # I notice that we have a few missing counts! Needs to be addressed when analysing these data.

plot(log(X270+1) ~ startDate, data=recs,type='l')  # I'm plotting log counts as the counts themselves vary wildly
lines(log(X304+1) ~ startDate, data=recs,type='l', col=2)  # I'm plotting log counts as the counts themselves vary wildly
