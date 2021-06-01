# Script to pull count data and run the JAGS models

rm(list=ls())

source("R/cwac_functions.r")

library(readxl)
library(tidyverse)

load("data/robertsDB.RData")
load("data/Barberspan_migrants.RData")
load("data/Barberspan_residents.Rdata")


african_grass_owl <- barberspan.res.counts[,c("360","startDate","season")]
owl.jags <- jags_analysis(african_grass_owl, 'north%20west', 'Barberspan', 1)

# Applying resident model to cormorant species ----------------------------
cormorant <- barberspan.res.counts[,c("47","startDate","season")]
comorant.jags <- jags_analysis(cormorant, 'north%20west', 'Barberspan', 1)

summary(comorant.jags)
comorant.jags

traceplot(comorant.jags, parameters = "sig.alpha")

# Applying "migrant" model to cormorant species -----------------------------
comorant.jags2 <- jags_analysis(cormorant, 'north%20west', 'Barberspan', 2)
comorant.jags2
traceplot(comorant.jags2, parameters = "mu_t")
traceplot(comorant.jags2, parameters = "mu_wt")
traceplot(comorant.jags2, parameters = "lambda")

# plot the summer and winter outputs
ts_jag_plot(comorant.jags2, cormorant, 2)

# Applying "migrant" model to cormorant species -----------------------------
stint <- barberspan.mig.counts[,c("253", "startDate", "season")]
stint.jags <- jags_analysis(stint, 'north%20west', 'Barberspan', 2)
stint.jags

# plot the summer and winter outputs
ts_jag_plot(stint.jags, stint, 2)

