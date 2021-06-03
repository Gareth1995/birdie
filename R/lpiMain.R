#' ---------------------------------------------------
#' Applying the LPI calculations to the CWAC database
#' Taking into account:
#' - Migratory status
#' - Nest site
#' - Foraging substratum
#' ---------------------------------------------------
library(dplyr)
library(jagsUI)

rm(list=ls())

source('R/calcIndex.R')
source('R/calcTrend.R')
source("R/cwac_functions.r")

load('data/robertsDB.RData')
load('data/barberspan_counts.RData')
load("data/Barberspan_migrants.RData")
load('data/Barberspan_residents.RData')


# ------------------------------------------
# Run jags analysis on all species from migrants and residents
# complete the data frame to include all years

# years <- barberspan.res.counts %>%
#   mutate(startDate = year(startDate)) %>%
#   filter(season != "O") %>%
#   complete(startDate = min(startDate):max(startDate), nesting(season)) %>%
#   select(startDate) %>%
#   unique()

resTrends <- list()
for (i in 1:(ncol(barberspan.res.counts)-2)){
  resTrends[[i]] <- jags_analysis(barberspan.res.counts[,c(i, 74, 73)],
                                    province = 'north%20west',
                                    site = 'Barberspan',
                                    spec_type = 2)$mean$beta
}

jags_analysis(barberspan.res.counts[,c(1, 74, 73)],
              province = 'north%20west',
              site = 'Barberspan',
              spec_type = 2)

specInd <- lapply(resTrends, calcIndex) # index array for each species

plot(specInd[[17]],
     x = unlist(as.list(years)),
     type = 'l',
     ylab = "Index",
     xlab = "Year",
     main = 'Cattle Egret')
# -------------------------------------------------------------------
# average trends per year across species
resTrends2 <- as.data.frame(resTrends)


# Calculate weights for each species --------------------------------

# sum of each bird species
resSums <- lapply(barberspan.res.counts[,1:72], sum, na.rm = T)
total <- sum(unlist(resSums))

# weights for each species
ws <- c()
for(i in 1:length(resSums)){
  ws <- c(ws, resSums[[i]]/total)
}

# Calculating weighted average trend for resident species ------------------
weightAvgs <- resTrends2 * t(ws)

yearAvgs <- apply(as.matrix(weightAvgs), 1, mean, na.rm = T)

resInd <- calcIndex(yearAvgs)

plot(resInd,
     x = unique(barberspan.res.counts$startDate),
     ylab = "Index",
     xlab = "Year",
     main = 'Residents')








# ------------------------------------------
# Calculate LPI for 1 species (great egret 58)

egret <- counts[,c("X245", "startDate", "season")]
egret.jags <- jags_analysis(egret, 'north%20west', 'Barberspan', 2)
ts_jag_plot(egret.jags, egret, 2)

egret.jags

# apply LPI to the trends from the jags model
lpis <- calcIndex(egret.jags$mean$beta)
mean(egret.jags$mean$beta)

egret$startDate <- year(egret$startDate)

egret <- egret[-which(egret$season == 'O'),]
egret <- egret %>% mutate(logCounts = log(egret[,1] + 1)) %>%
  complete(startDate = min(startDate):max(startDate), nesting(season))
plot(y = lpis, x = unique(egret$startDate))



