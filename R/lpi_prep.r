# ---------------------------------------------------------------------
# Script to create a database that can be used for the index creation.
# Data will have the necessary grouping variables for group indeces and
# upwards aggregation to location level.
# ---------------------------------------------------------------------
rm(list=ls())

source('R/cwac_functions.r')

library(writexl)
library(readxl)
library(dplyr)

robertsDB <- read_excel('data/Roberts DB WRJD.xls')
barberspan_species <- read_excel('data/barberspanIDs.xlsx')
barberScientificName <- get_local_species('north%20west', 'Barberspan')

barberspan_species <- left_join(barberScientificName, barberspan_species)

# delete first 8 rows
robertsDB <- robertsDB[9:nrow(robertsDB) , ]

# extract relevant cols from the robertsDB
robertsDB <- left_join(barberspan_species, robertsDB) %>% 
  select(id, `Common name`, `Scientific name`,
         Family, Migrant, `Nest site`,
         `Foraging substratum`)

# remove erroneous rows due to join -----------------------------------------

# remove African Pied wagtail (685) with nest site NA
robertsDB <- robertsDB[-which((robertsDB$id == "685") & (is.na(robertsDB$`Nest site`))),]

# remove grey wagtail (690) with foraging substratum NA
robertsDB <- robertsDB[-which((robertsDB$id == "690") & (is.na(robertsDB$`Foraging substratum`))),]

# create a new column that aggregates the foraging types (how many different foraging techniques a species knows)


save(robertsDB, file = 'data/robertsDB.RData')



