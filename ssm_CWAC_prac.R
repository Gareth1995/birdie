library(coda)
library(rjags)
library(runjags)
library(ggplot2)
library(lubridate)
library(dplyr)
library(schoolmath)

#setwd("D:/Thesis/birdie")
# load functions needed to download CAWC data
source("cwac api data download functions.r")

# Applying Bayesian state space time series model with JAGS and using CWAC data
# Using 1 resident specie

#all_sites <- c('western%20cape', 'northern%20cape', 'mpumalanga', 'north%20west',
               #'gauteng', 'free%20state') # dont know the structure to these strings
                                          # missing sites

# get all cards for a given site
get_local_cards <- function(province, location){
  sites <- get_cwac_sites(province = province)
  if(length(sites$Loc_code[sites$Name==location]) != 0){
    
    cards <- get_cwac_cards(site=sites$Loc_code[sites$Name==location])
    cards$startDate <- as.Date(cards$startDate)
    
    return(cards)
    
  }else{
    print("No such location exists in the dataset")
  }
}


# Function that takes location code and extracts all bird species in that area
get_local_species <- function(province, location){
    
  sites <- get_cwac_sites(province = province)
  loc_code <- sites$Loc_code[sites$Name==location]
  
  url = paste0('http://api.adu.org.za/cwac/cards/single/list?locationcode=',loc_code)
  
  myfile <- getURL(url, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  json_file <- rjson::fromJSON(myfile)
  
  # changing nulls in the Json files to NAs
  json_file <- lapply(json_file, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  
  df <- as.data.frame(do.call("rbind", json_file))
  
  sdf <- stack(df)
  sdf$ind <- as.character(sdf$ind)
  
  species <- as.data.frame(sdf[which(startsWith(sdf$ind, 'list.common')),'values'])
  specIds <- as.data.frame(sdf[which(startsWith(sdf$ind, 'list.ref')),'values'])
  
  colnames(species) <- "species"
  colnames(specIds) <- "id"
  
  specList <- cbind(species, specIds)
  
  return(specList)
  
}

# obtain counts of specified species in area of interest #
species_counts <- function(province, location, species){
  
  cards <- get_local_cards(province, location)
  
  # setting up the data frame to be populated
  recs <- data.frame(matrix(nrow=dim(cards)[1], ncol = length(species)))
  
  # populate the data frame
  for (i in 1:dim(cards)[1]) {
    # get records from each card in the specified site
    cd <- get_cwac_records(card=cards$Card[i])
    
    for (j in 1:length(species)) {
      # get counts of specified species in that card
      x <- as.numeric(as.character(cd$count[which(cd$spp == species[j])]))
      if (length(x)==1) recs[i,j] <- x
    }
  }
  # order data dframe by date
  spec_counts <- cbind(recs, cards$startDate, cards$Season)
  colnames(spec_counts) <- c(paste("X",species, sep=''), "startDate", "season")
  spec_counts$startDate <- lubridate::ymd(spec_counts$startDate)
  
  
  return(dplyr::arrange(spec_counts, startDate))
}

#### Model 1 (common resident species) ####
jags_analysis <- function(spec_counts, province, site, spec_type){
  # spec_type: 1. common resident
  #            2. common migrant
  #            3. rare resident
  #            4. rare migrant
  
  cards <- get_local_cards(province, site)
  cards$Season[cards$Season == 'S'] = 1
  cards$Season[cards$Season == 'W'] = 2
  
  # change NAs to 0s
  spec_counts[is.na(spec_counts)] <- 0 
  
  # get length of counts
  n <- length(spec_counts)
  
  # log counts for log scale in jags
  spec_counts <- log(spec_counts)
  spec_counts[is.infinite(spec_counts)] <- 0
  
  if(spec_type == 1){
    # data list for jags analysis
    data_jags <- list(y=spec_counts, n=n, x=as.numeric(cards$Season))
    
    # variables to be tracked
    params <- c('mu_t')
    
    # running the model
    jag.mod <- run.jags('cwac.JAGS',
                        data = data_jags,
                        monitor = params,
                        #inits = inits,
                        n.chains = 3,
                        adapt = 2000,
                        burnin = 5000,
                        sample = 2000,
                        thin = 200)
  }
  else if(spec_type == 2){
    
    # data list for jags analysis
    data_jags <- list(s = spec_df[which(spec_df$season == 'S')][,1],
                      w = spec_df[which(spec_df$season == 'W')][,1],
                      n = n)
    
    # variables to be tracked
    params <- c('mu_t', 'lambda')
    
    # running the model
    jag.mod <- run.jags('cwac_com_migrant.jags',
                        data = data_jags,
                        monitor = params,
                        #inits = inits,
                        n.chains = 3,
                        adapt = 2000,
                        burnin = 5000,
                        sample = 2000,
                        thin = 200)
  }
  
  
  return(jag.mod)
}

ts_jag_plot <- function(jag.model){
  
  est.pop <- summary(jag.mod)[,"Median"]
  lower95 <- summary(jag.mod)[,"Lower95"]
  upper95 <- summary(jag.mod)[,"Upper95"]
  
  ## Empty vectors to store the data
  ssm_sim1 <- data.frame(Year = scount$startDate, 
                         estimated = est.pop,
                         obs = counts,
                         lower = lower95,
                         upper = upper95)
  
  ssm_sim1 <- arrange(ssm_sim1, Year)
  
  # plotting the observed and estimated population sizes produced by the state process
  ggplot(ssm_sim1, aes(x = Year)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
    geom_line(aes(y = estimated, color = "grey50"), lwd = 1) +
    geom_line(aes(y = obs, color = "red"), lwd = 1) +
    scale_color_identity(guide = "legend",
                         name = "Legend",
                         labels = c("State process", "Observed")) + 
    
    labs(x = "Year",
         y = "Population") 
}

#### Applying the functions ####

# get the different species in the location
Kamf_species <- get_local_species('northern%20cape', 'Kamfers Dam')
nrow(Kamf_species) #  number of species in the Kamfers Dam location

# obtain count from selected specie in location
scount <- species_counts('northern%20cape', 'Kamfers Dam', Kamf_species[18,2])

# run jags analysis on the counts
jag.model <- jags_analysis(scount$X87, 'northern%20cape', 'Kamfers Dam', 1)



#### Other metrics of jags model ####

# showing the convergence of 1st estimated observation for the first chain
plot(jag.mod$mcmc[[1]][,1]) 

# using gelman-Rubin diagnostics for convergence checking
gelman.diag(jag.mod$mcmc)
gelman.plot(jag.mod$mcmc)

#### Model 2 (common migratory species) ####

deHoop_specs <- get_local_species('western%20cape', 'De Hoop Vlei')
#dehoop_costal_specs <- get_local_species('western%20cape', 'De Hoop Coastal')

# using great white pelican to test model 2
gwPelican <- species_counts('western%20cape', 'De Hoop Vlei', deHoop_specs[deHoop_specs$id == 42,]$id)
gwPelican <- arrange(gwPelican, startDate)

# change NAs to 0s
gwPelican[is.na(gwPelican)] <- 0

# log counts for log scale in jags
gwPelican <- mutate(gwPelican, logCounts = log(gwPelican[,1]))
gwPelican$logCounts[is.infinite(gwPelican$logCounts)] <- 0

# get summer and winter count lengths
summer <- gwPelican[which(gwPelican$season == 'S'),]
winter <- gwPelican[which(gwPelican$season == 'W'),]
slength <- nrow(summer)
wlength <- nrow(winter)

# data list for jags analysis
data_jags <- list(summer = summer[,'logCounts'],
                  winter = winter[,'logCounts'],
                  slength = slength,
                  wlength = wlength,
                  n = max(slength, wlength))

data_jags

# variables to be tracked
params <- c('mu_t', 'lambda')

# running the model
jag.mod <- run.jags('cwac_com_migrant.jags',
                    data = data_jags,
                    monitor = params,
                    #inits = inits,
                    n.chains = 3,
                    adapt = 2000,
                    burnin = 5000,
                    sample = 2000,
                    thin = 200)
pelican_summary <- summary(jag.mod)
pelican_summary

# plotting the model

est.pop <- pelican_summary[,"Median"]
lower95 <- pelican_summary[,"Lower95"]
upper95 <- pelican_summary[,"Upper95"]

# storing data in data frames
summerdf <- data.frame(Year = summer$startDate,
                       s_estimated = est.pop[1:25],
                       s_counts = summer$logCounts)
                       
winterdf <- data.frame(Year = winter$startDate, 
                       lambda = est.pop[26:45],
                       s_estimated = est.pop[1:20],
                       w_counts = winter$logCounts)
                       #lower = lower95,
                       #upper = upper95)
winterdf <- winterdf %>% 
  mutate(w_estimated = s_estimated + lambda)

summerdf <- arrange(summerdf, Year)
winterdf <- arrange(winterdf, Year)


# plotting the observed and estimated population sizes produced by the state process
ggplot(winterdf, aes(x = Year)) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
  geom_line(aes(y = w_estimated, color = "grey50"), lwd = 1) +
  geom_line(aes(y = w_counts, color = "red"), lwd = 1) +
  scale_color_identity(guide = "legend",
                       name = "Legend",
                       labels = c("State process", "Observed")) + 
  
  labs(x = "Year",
       y = "Population")






















