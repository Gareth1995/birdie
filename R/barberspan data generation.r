# Saving data for the barberspan location

library(writexl)
library(readxl)
library(dplyr)
library(stringr)

rm(list=ls())

source("R/cwac_functions.r")

# shows species in barberspan area and if species are migrants or residents

load('data/robertsDB.RData')
load("data/barberspan_counts.RData")

# ----------------------------------------------------------------------------
# Only run once

# getting all counts from barberspan
species <- get_local_species('north%20west', 'Barberspan')

counts <- species_counts('north%20west', 'Barberspan', species$id)
counts <- counts[, order(ncol(counts):1)]
counts <- as.data.frame(counts)

# save counts
save(counts, file = "data/barberspan_counts.RData")
# -----------------------------------------------------------------------------

# grouping migrant status

barberspan.resident <- robertsDB[which(robertsDB$Migrant == "n"),]
barberspan.migrant <- anti_join(robertsDB, barberspan.resident, by = "Scientific name")

# populating the resident species counts
barberspan.res.counts <- counts %>% select(barberspan.resident$id, startDate, season)
save(barberspan.res.counts, file = "data/Barberspan_residents.RData")

# populating the migrants species counts
barberspan.mig.counts <- counts %>% select(barberspan.migrant$id, startDate, season)
save(barberspan.mig.counts, file = "data/Barberspan_migrants.RData")


# Grouping nest sites

# fill in missing nest site data
robertsDB[which(robertsDB$`Common name` == 'Eurasian Curlew'),]$`Nest site` <- 'Grass'
robertsDB[which(robertsDB$`Common name` == 'African Darter'),]$`Nest site` <- 'Diverse'
robertsDB[which(robertsDB$`Common name` == 'Bar-tailed Godwit'),]$`Nest site` <- 'Shrub'
robertsDB[which(robertsDB$`Common name` == 'Black-tailed Godwit'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Common Greenshank'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Common Black-headed Gull'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Montagus Harrier'),]$`Nest site` <- 'Diverse'
robertsDB[which(robertsDB$`Common name` == 'Black-headed Heron'),]$`Nest site` <- 'Tree'
robertsDB[which(robertsDB$`Common name` == 'African Sacred Ibis'),]$`Nest site` <- 'Tree'
robertsDB[which(robertsDB$`Common name` == 'Osprey'),]$`Nest site` <- 'Diverse'
robertsDB[which(robertsDB$`Common name` == 'Pink-backed Pelican'),]$`Nest site` <- 'Tree'
robertsDB[which(robertsDB$`Common name` == 'Caspian Plover'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Common Ringed Plover'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Greater Sand Plover'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Grey Plover'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Black-winged Pratincole'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Ruff'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Sanderling'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Buff-breasted Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Common Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Curlew Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Green Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Marsh Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Terek Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Wood Sandpiper'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Little Stint'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Caspian Tern'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'White-winged Tern'),]$`Nest site` <- 'Diverse'
robertsDB[which(robertsDB$`Common name` == 'Ruddy Turnstone'),]$`Nest site` <- 'Ground'
robertsDB[which(robertsDB$`Common name` == 'Grey Wagtail'),]$`Nest site` <- 'Diverse'
robertsDB[which(robertsDB$`Common name` == 'Yellow Wagtail'),]$`Nest site` <- 'Grass'
robertsDB[which(robertsDB$`Common name` == 'Cape Cormorant'),]$`Nest site` <- 'Diverse'
robertsDB$`Nest site`[is.na(robertsDB$`Nest site`)] <- ''

# species divided into nest type
ground <- robertsDB %>% filter(`Nest site`== 'Ground')
aquatic <- robertsDB %>% filter(`Nest site`== 'Aquatic')
cavity <- robertsDB %>% filter(`Nest site`== 'Cavity')
diverse <- robertsDB %>% filter(`Nest site`== 'Diverse')
grass <- robertsDB %>% filter(`Nest site`== 'Grass')
niche <- robertsDB %>% filter(`Nest site`== 'Niche')
reed <- robertsDB %>% filter(`Nest site`== 'Reed')
shrub <- robertsDB %>% filter(`Nest site`== 'Shrub')
tree <- robertsDB %>% filter(`Nest site`== 'Tree')

# get counts
ground <- counts %>% select(ground$id, startDate, season)
aquatic <- counts %>% select(aquatic$id, startDate, season)
cavity <- counts %>% select(cavity$id, startDate, season)
diverse <- counts %>% select(diverse$id, startDate, season)
grass <- counts %>% select(grass$id, startDate, season)
niche <- counts %>% select(niche$id, startDate, season)
reed <- counts %>% select(reed$id, startDate, season)
shrub <- counts %>% select(shrub$id, startDate, season)
tree <- counts %>% select(tree$id, startDate, season)

# save counts for later analysis
save(ground, file = "data/ground.RData")
save(aquatic, file = "data/aquatic.RData")
save(cavity, file = "data/cavity.RData")
save(diverse, file = "data/diverse.RData")
save(grass, file = "data/grass.RData")
save(niche, file = "data/niche.RData")
save(reed, file = "data/reed.RData")
save(shrub, file = "data/shrub.RData")
save(tree, file = "data/tree.RData")


# classifying species according to how many different types of foriging substratum they possess

robertsDB <- robertsDB %>%
  mutate(numForage = (str_count(`Foraging substratum`, '[0-9]')))

counts[,robertsDB[which(robertsDB$numForage == 1),]$id]


