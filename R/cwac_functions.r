# functions to pull necessary data from the CWAC database

library(ggplot2)
library(lubridate)
library(dplyr)
library(jagsUI)
library(gridExtra)
library(tidyr)
library(cowplot)

source("R/cwac api data download functions.r")

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
  taxonomic <- as.data.frame(sdf[which(startsWith(sdf$ind, 'list.taxonomic')),'values'])
  
  colnames(species) <- "species"
  colnames(specIds) <- "id"
  colnames(taxonomic) <- "Scientific name"
  
  specList <- cbind(species, specIds, taxonomic)
  
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
  colnames(spec_counts) <- c(paste(species, sep=''), "startDate", "season")
  spec_counts$startDate <- year(spec_counts$startDate)
  
  # complete the data frame to include all years
  #$startDate <- year(bird_df$startDate)
  
  spec_counts <- spec_counts[-which(spec_counts$season == 'O'),]
  spec_counts <- spec_counts %>% tidyr::complete(startDate = min(startDate):max(startDate), nesting(season))
  
  
  return(dplyr::arrange(spec_counts, startDate))
}

#### JAGS method to run analysis on the 4 different species types ####
jags_analysis <- function(bird_df){
  
  # convert to log counts for jags
  bird_df <- bird_df %>% 
    mutate(logCounts = log(bird_df[,1] + 1))
    
  # get summer and winter count lengths
  summer <- bird_df[which(bird_df$season == 'S'),]
  winter <- bird_df[which(bird_df$season == 'W'),]
    
  # data list for jags analysis
  data_jags <- list(summer = summer$logCounts,
                    winter = winter$logCounts,
                    N = nrow(bird_df)/2)
    
  # variables to be tracked
  params <- c('mu_t', 'mu_wt', 'lambda', 'beta', 'winter', 'summer')
    
  # running the model
  jag.mod <- jags(data = data_jags,
                  parameters.to.save = params,
                  model.file = 'JAGS/cwac_ssm_migrant.jags',
                  n.chains = 3,
                  n.iter = 10000,
                  n.burnin = 5000,
                  n.thin = 1,
                  modules = c('glm','lecuyer', 'dic'),
                  factories = NULL,
                  parallel = T,
                  n.cores = 3,
                  DIC = TRUE,
                  verbose = TRUE)
  
  
  return(jag.mod)
}


ts_jag_plot <- function(jag.model, bird_df, spec_type){
  
  bird_df <- mutate(bird_df, logCounts = log(bird_df[,1] + 1))
  
  if(spec_type == 1){
    
    sEstimated <- jag.model$mean$mu_t
    sLower <- jag.model$q2.5$mu_t
    sUpper <- jag.model$q97.5$mu_t
    
    ## Empty vectors to store the data
    ssm_sim1 <- data.frame(Year = bird_df$startDate, 
                           estimated = sEstimated,
                           obs = bird_df$logCounts,
                           lower = sLower,
                           upper = sUpper,
                           season = bird_df$season)
    
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
           y = "Population") +
      facet_wrap(~ season, ncol = 1,)
  }
  else if(spec_type == 2){
    
    sEstimated <- jag.model$mean$mu_t
    sLower <- jag.model$q2.5$mu_t
    sUpper <- jag.model$q97.5$mu_t
    
    # separating the summer and winter counts
    summer <- bird_df[which(bird_df$season == 'S'),]
    winter <- bird_df[which(bird_df$season == 'W'),]
    
    # storing data in data frames
    summerdf <- data.frame(Year = summer$startDate,
                           s_estimated = sEstimated,
                           s_counts = summer$logCounts,
                           lower = sLower,
                           upper = sUpper)
    
    lambda <- jag.model$mean$lambda
    wEstimated <- jag.model$mean$mu_wt
    wLower <- jag.model$q2.5$mu_wt
    wUpper <- jag.model$q97.5$mu_wt
    
    winterdf <- data.frame(Year = winter$startDate, 
                           lambda = lambda,
                           w_estimated = wEstimated,
                           w_counts = winter$logCounts,
                           lower = wLower,
                           upper = wUpper)
    
    
    summerdf <- arrange(summerdf, Year)
    winterdf <- arrange(winterdf, Year)
    
    # plotting the observed and estimated population sizes produced by the state process
  
    # summer
    summer_plot <- ggplot(summerdf, aes(x = Year)) +
      
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
      geom_line(aes(y = s_estimated, color = "grey1"), lwd = 1, lty = 2) +
      geom_line(aes(y = s_counts, color = "red"), lwd = 1 ) +
      scale_color_identity(guide = "legend",
                           name = "",
                           labels = c("State process", "Log Counts")) +
      labs(title = "Summer") +
      theme(axis.title.x=element_blank(),
            axis.title.y = element_blank()) 
    
      
      
    # winter
    winter_plot <- ggplot(winterdf, aes(x = Year)) +
      
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
      geom_line(aes(y = w_estimated, color = "gray1"), lwd = 1, lty = 2) +
      geom_line(aes(y = w_counts, color = "blue"), lwd = 1) +
      scale_color_identity(guide = "legend",
                           name = "",
                           labels = c("Log Counts", "State Process")) +
      labs(title = "Winter") +
      theme(axis.title.x=element_blank(),
            axis.title.y = element_blank())
      
    
    grid.arrange(summer_plot, winter_plot,
                 nrow = 2, heights = c(1/2, 1/2),
                 left = "Log Population",
                 bottom = "Year")
    
  }
}

#### hill numbers jags test ####
# inputs: group of species counts, season vector
hill_nums <- function(groupdf){
  
  # TEST VARS
  #groupdf <- counts
  
  # calculate the number of species in the group
  specnum <- ncol(groupdf[,-c(ncol(groupdf), ncol(groupdf)-1)])
  
  # extract the species from the group df according to season
  summer <- groupdf[which(groupdf$season == 'S'), 1:specnum]
  winter <- groupdf[which(groupdf$season == 'W'), 1:specnum]
  
  # place species in a matrix
  summer <- log(as.matrix(summer))
  winter <- log(as.matrix(winter))
  
  # data list for jags analysis
  data_jags <- list("numspecs" = specnum,
                    "N" = nrow(groupdf)/2,
                    "summer" = summer,
                    "winter" = winter)
  
  # variables to be tracked
  params <- c("mu_t", "mu_wt", "sig.w2", "sig.eps2",
              "sig.alpha", "beta", "lambda",
              "summer_props", "winter_props",
              "summer_n0", "winter_n0",
              "summer_n1", "winter_n1",
              "summer_n2", "winter_n2")
  
  # run the model
  jag.mod <- jags(data = data_jags,
                  parameters.to.save = params,
                  model.file = 'JAGS/hillnum.jags',
                  n.chains = 3,
                  n.iter = 10000,
                  n.burnin = 5000,
                  n.thin = 1,
                  #modules = c('glm','lecuyer', 'dic'),
                  modules = NULL,
                  factories = NULL,
                  parallel = T,
                  n.cores = 3,
                  DIC = TRUE,
                  verbose = TRUE)
  
  return(jag.mod)
}

plot_hills <- function(jag.mod, years){
  
  # plotting shannons equitability instead of the actual shannons index
  # using exp for effective number of species (true diversity value)
  # shannons equ for summer
  numSpecs <- log(ncol(jag.mod$mean$mu_t))
  sn1_df <- data.frame(years = years,
                      estimated = jag.mod$mean$summer_n1/numSpecs,
                      lower = jag.mod$q2.5$summer_n1/numSpecs,
                      upper = jag.mod$q97.5$summer_n1/numSpecs,
                      h = exp(jag.mod$mean$summer_n1),
                      lowerh = exp(jag.mod$q2.5$summer_n1),
                      upperh = exp(jag.mod$q97.5$summer_n1))
  
  # shannons equ for winter
  wn1_df <- data.frame(years = years,
                       estimated = jag.mod$mean$winter_n1/numSpecs,
                       lower = jag.mod$q2.5$winter_n1/numSpecs,
                       upper = jag.mod$q97.5$winter_n1/numSpecs,
                       h = exp(jag.mod$mean$winter_n1),
                       lowerh = exp(jag.mod$q2.5$winter_n1),
                       upperh = exp(jag.mod$q97.5$winter_n1))
  
  
  # plotting 1/Simpsons index for true diversity value
  # simpsons index for summer
  sn2_df <- data.frame(years = years,
                      estimated = jag.mod$mean$summer_n2,
                      lower = jag.mod$q2.5$summer_n2,
                      upper = jag.mod$q97.5$summer_n2)
  
  # simpsons index for winter
  wn2_df <- data.frame(years = years,
                      estimated = jag.mod$mean$winter_n2,
                      lower = jag.mod$q2.5$winter_n2,
                      upper = jag.mod$q97.5$winter_n2)
  
  # create the plots
  sn1_plot <- ggplot(sn1_df, aes(x = years)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
    geom_line(aes(y = estimated, color = "grey1"), lwd = 1, lty = 2) +
    geom_line(aes(y = h, color = 'red'), lwd = 1, lty = 1) +
    geom_line(aes(y = lowerh, colour = 'red'), lwd = 1, lty = 3) +
    geom_line(aes(y = upperh, colour = 'red'), lwd = 1, lty = 3) +
    
    scale_color_identity(guide = "legend",
                         name = "",
                         labels = c("Equitability", "H")) +
    
    labs(title = "Shannons Index(S)")
  
  wn1_plot <- ggplot(wn1_df, aes(x = years)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
    geom_line(aes(y = estimated, color = "grey1"), lwd = 1, lty = 2) +
    geom_line(aes(y = h, color = 'red'), lwd = 1, lty = 1) +
    
    geom_line(aes(y = lowerh, colour = 'red'), lwd = 1, lty = 3) +
    geom_line(aes(y = upperh, colour = 'red'), lwd = 1, lty = 3) +
    
    scale_color_identity(guide = "legend",
                         name = "",
                         labels = c("Equitability", "H")) +
    
    labs(title = "Shannons Index (W)")
  
  sn2_plot <- ggplot(sn2_df, aes(x = years)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
    geom_line(aes(y = estimated, color = "grey1"), lwd = 1, lty = 2) +
    labs(title = "Simpsons Index (S)")
  
  wn2_plot <- ggplot(wn2_df, aes(x = years)) +
    
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "gray80") +
    geom_line(aes(y = estimated, color = "grey1"), lwd = 1, lty = 2) +
    labs(title = "Simpsons Index (W)")
  
  return(plot_grid(sn1_plot, wn1_plot,
            sn2_plot, wn2_plot,
            ncol = 2, nrow = 2))
  
  
  
}

# Testing methods
#hill <- hill_nums(counts)

#plot_hills(hill, unique(counts$startDate))
