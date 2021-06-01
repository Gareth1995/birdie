# Saving data for the barberspan location

library(writexl)
library(readxl)
library(dplyr)

rm(list=ls())

#source("R/cwac api data download functions.r")

# shows species in barberspan area and if species are migrants or residents

load('data/robertsDB.RData')
load("data/barberspan_counts.RData")

# species divided into migrant status
barberspan.resident <- robertsDB[which(robertsDB$Migrant == "n"),]
barberspan.migrant <- anti_join(robertsDB, barberspan.resident, by = "Scientific name")


# populating the resident species counts
barberspan.res.counts <- data.frame()
for(i in 1:(ncol(b.spec.counts)-2)){
  if(robertsDB$Migrant[i] == "n"){
    species.counts <- b.spec.counts[,i]
  
    if(ncol(barberspan.res.counts) == 0){
      barberspan.res.counts <- as.data.frame(species.counts)
    }
    else{
      barberspan.res.counts <- cbind(barberspan.res.counts, species.counts)
    }
  }
}

colnames(barberspan.res.counts) <- barberspan.resident$id
barberspan.res.counts <- cbind(barberspan.res.counts, b.spec.counts[,c(107,108)])

save(barberspan.res.counts, file = "Barberspan_residents.RData")

# populating the migrants species counts
barberspan.mig.counts <- data.frame()
for(i in 1:(ncol(b.spec.counts) - 2)){
  if(robertsDB$Migrant[i] == "y"){
    species.counts <- b.spec.counts[,i]
    
    if(ncol(barberspan.mig.counts) == 0){
      barberspan.mig.counts <- as.data.frame(species.counts)
    }
    else{
      barberspan.mig.counts <- cbind(barberspan.mig.counts, species.counts)
    }
  }
}

colnames(barberspan.mig.counts) <- barberspan.migrant$id
barberspan.mig.counts <- cbind(barberspan.mig.counts, b.spec.counts[,c(107,108)])
save(barberspan.mig.counts, file = "Barberspan_migrants.RData")

# populating different types of nest sites
ground <- data.frame()
for(i in 1:(ncol(b.spec.counts) - 2)){
  if(robertsDB$`Nest site`[i] == "Ground"){
    gBird <- b.spec.counts[,i]
  
    if(ncol(ground) == 0){
      ground <- as.data.frame(gBird)
      colnames(ground) <- colnames(b.spec.counts[i])
    }
    else{
      ground <- cbind(ground, gBird)
    }
  }
}






