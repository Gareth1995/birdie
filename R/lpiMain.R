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


# Applying the LPI to species grouped by migratory status --------------------
red_com <- calcTrend(b.spec.counts$'212')
redCom_ind <- calcIndex(red_com)

plot(coot_trend, x = b.spec.counts$startDate[2:nrow(b.spec.counts)])
plot(redCom_ind, x = b.spec.counts$startDate[2:nrow(b.spec.counts)], type = 'l')

# group by Nest site
