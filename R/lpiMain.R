#' ---------------------------------------------------
#' Applying the LPI calculations to the CWAC database
#' Taking into account:
#' - Migratory status
#' - Nest site
#' - Foraging substratum
#' ---------------------------------------------------
library(dplyr)

rm(list=ls())

source('R/calcIndex.R')
source('R/calcTrend.R')
source("R/cwac_functions.r")

load('data/robertsDB.RData')
load('data/barberspan_counts.RData')
load("data/Barberspan_migrants.RData")
load('data/Barberspan_residents.RData')

# removing species with fewer than 6 observations
nas <- as.vector(apply(barberspan.res.counts, 2, function(x){
  return(sum(!is.na(x)))
  }))

mignas <- as.vector(apply(barberspan.mig.counts, 2, function(x){
  return(sum(!is.na(x)))
}))


# species with 6 or more counts
residents <- barberspan.res.counts[,nas>=6]
migrants <- barberspan.mig.counts[,mignas>=6]


# ------------------------------------------
# Run jags analysis on all species from migrants and residents

resTrends <- list()
for (i in 1:ncol(residents)){
  resTrends[[i]] <- jags_analysis(residents[,c(i, (ncol(residents)-2), (ncol(residents)-1))],
                                  province = 'north%20west',
                                  site = 'Barberspan',
                                  spec_type = 2)
}

# save jags analysis for resTrends
save(resTrends, file = "data/resJagsAnalysis.RData")

migTrends <- list()
for(i in 1:(ncol(migrants)-2)){
  migTrends[[i]] <- jags_analysis(migrants[,c(i, (ncol(migrants)-2), (ncol(migrants)-1))],
                                  province = 'north%20west',
                                  site = 'Barberspan',
                                  spec_type = 2)
}

# -------------------------------------------------------------------
# function to calculate yearly indices for a group
getIndex <- function(trends, groupdf, n){
  # trends <- resTrends
  # groupdf <- residents[,1:44]
  # n <- 1

  # get list of betas
  betas <- as.data.frame(lapply(trends, function(x){
    return(x$mean$beta)
  }))
  
  # get list of beta 2.5
  betaLower <- as.data.frame(lapply(trends, function(x){
    return(x$q2.5$beta)
  }))
  
  # get list of beta 97.5
  betaUpper <- as.data.frame(lapply(trends, function(x){
    return(x$q97.5$beta)
  }))
  
  # average trends per year across species
  colnames(betas) <- colnames(groupdf)
  colnames(betaLower) <- colnames(groupdf)
  colnames(betaUpper) <- colnames(groupdf)
  
  # Calculate weights for each species #
  
  # sum of each bird species
  resSums <- lapply(groupdf, sum, na.rm = T)
  total <- sum(unlist(resSums))
  
  # weights for each species
  ws <- c()
  for(i in 1:length(resSums)){
    ws <- c(ws, resSums[[i]]/total)
  }
  
  # Calculating weighted average trend for resident species
  weightAvgs <- as.data.frame(t(t(betas) * ws))
  weightAvgsl <- as.data.frame(t(t(betaLower) * ws))
  weightAvgsu <- as.data.frame(t(t(betaUpper) * ws))
  
  # group the avgs into groups of n (i.e average trend over n year periods)
  yearnavg <- list()
  yearnavgl <- list()
  yearnavgu <- list()
  for(i in (seq(from = 1, to = 26, by = n))){
    yearnavg[[i]] <- mean(unlist(apply(weightAvgs[i:(i+(n-1)), ], 2, mean)))
    yearnavgl[[i]] <- mean(unlist(apply(weightAvgsl[i:(i+(n-1)), ], 2, mean)))
    yearnavgu[[i]] <- mean(unlist(apply(weightAvgsu[i:(i+(n-1)), ], 2, mean)))
  }
  
  # remove nulls
  names(yearnavg) <- seq_along(yearnavg)
  names(yearnavgl) <- seq_along(yearnavgl)
  names(yearnavgu) <- seq_along(yearnavgu)
  
  yearnavg <- Filter(Negate(is.null),yearnavg)
  yearnavgl <- Filter(Negate(is.null),yearnavgl)
  yearnavgu <- Filter(Negate(is.null),yearnavgu)
  
  yearnavg <- lapply(yearnavg, calcIndex)
  yearnavgl <- lapply(yearnavgl, calcIndex)
  yearnavgu <- lapply(yearnavgu, calcIndex)
  
  # bind the beta values (lower and upper bounds) to a data frame
  resInd <- cbind(unlist(yearnavgl), unlist(yearnavg), unlist(yearnavgu))
  resInd <- rbind(rep(1, ncol(resInd)), resInd)
  
  # get index for every 5 year period
  #resInd <- apply(yearnavg, c(1,2), calcIndex)
  
  return(as.data.frame(resInd)) # the group index for each n year period
}

resInd <- getIndex(resTrends, residents[,1:44], 1)

ggplot(resInd, aes(x = unique(barberspan.res.counts$startDate)[seq(from = 1, to = 27, by = 1)])) +
  
  geom_ribbon(aes(ymin = V1, ymax = V3), fill = "gray80") +
  geom_line(aes(y = V2, color = "grey1"), lwd = 1, lty = 1)

migInd <- getIndex(migTrends, migrants[,1:25], 1)

ggplot(migInd, aes(x = unique(barberspan.res.counts$startDate)[seq(from = 1, to = 27, by = 1)])) +
  
  geom_ribbon(aes(ymin = V1, ymax = V3), fill = "gray80") +
  geom_line(aes(y = V2, color = "grey1"), lwd = 1, lty = 1)







# # average trends per year across species
# resTrends2 <- as.data.frame(resTrends)


# # Calculate weights for each species --------------------------------
# 
# # sum of each bird species
# resSums <- lapply(barberspan.res.counts[,1:72], sum, na.rm = T)
# total <- sum(unlist(resSums))
# 
# # weights for each species
# ws <- c()
# for(i in 1:length(resSums)){
#   ws <- c(ws, resSums[[i]]/total)
# }
# 
# # Calculating weighted average trend for resident species ------------------
# weightAvgs <- resTrends2 * t(ws)
# 
# yearAvgs <- apply(as.matrix(weightAvgs), 1, mean, na.rm = T)
# 
# resInd <- calcIndex(yearAvgs)
# 
# plot(resInd,
#      x = unique(barberspan.res.counts$startDate),
#      ylab = "Index",
#      xlab = "Year",
#      main = 'Residents')








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



