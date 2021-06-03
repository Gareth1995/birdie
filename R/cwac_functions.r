# functions to pull necessary data from the CWAC database

library(ggplot2)
library(lubridate)
library(dplyr)
library(jagsUI)
library(gridExtra)
library(tidyr)

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
  colnames(spec_counts) <- c(paste("X",species, sep=''), "startDate", "season")
  spec_counts$startDate <- year(spec_counts$startDate)
  
  # complete the data frame to include all years
  #$startDate <- year(bird_df$startDate)
  
  spec_counts <- spec_counts[-which(spec_counts$season == 'O'),]
  spec_counts <- spec_counts %>% complete(startDate = min(startDate):max(startDate), nesting(season))
  
  
  return(dplyr::arrange(spec_counts, startDate))
}

#### JAGS method to run analysis on the 4 different species types ####
jags_analysis <- function(bird_df, province, site, spec_type){
  # spec_type: 1. common resident
  #            2. common migrant
  #            3. rare resident
  #            4. rare migrant
  
  # complete the data frame to include all years
  #bird_df$startDate <- year(bird_df$startDate)
  
  #bird_df <- bird_df[-which(bird_df$season == 'O'),]
  bird_df <- bird_df %>% 
    mutate(logCounts = log(bird_df[,1] + 1))
    
    #complete(startDate = min(startDate):max(startDate), nesting(season))
  
  if(spec_type == 1){
    
    # encode summer and winter to 1 and 2 respectively
    bird_df$season[bird_df$season == 'S'] <- 1
    bird_df$season[bird_df$season == 'W'] <- 2
    
    # removing out of season counts
    #bird_df <- bird_df[-which(bird_df$season == 'O'),]
    
    # change NAs to 0s
    #spec_counts <- bird_df[,1]
    #spec_counts[is.na(spec_counts)] <- 0 
    
    # get length of counts
    n <- nrow(bird_df)
    
    # log counts for log scale in jags
    #spec_counts <- log(spec_counts)
    #spec_counts[is.infinite(spec_counts)] <- 0
    
    # Inits function
    mod.inits <- function(){
      list ("mu_t[1]" = runif(1, log(1),log(1500)),
            "beta[1]" = runif(1, log(1),log(100)),
            "tau.w2" = runif(1, log(1),log(5)),
            "tau.eps2" = runif(1, log(1),log(5)),
            "tau.alpha" = runif(1, log(1),log(5)))
      }
    
    # data list for jags analysis
    data_jags <- list(y=bird_df$logCounts, n=n, x=as.numeric(bird_df$season))
    
    # variables to be tracked
    params <- c("mu_t", "sig.w2", "sig.eps2", "sig.alpha", "beta", "y")
    
    # running the model
    jag.mod <- jags(data = data_jags,
                    inits = mod.inits,
                    parameters.to.save = params,
                    model.file = 'JAGS/cwac_com_resident.jags',
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
    
  }else if(spec_type == 2){
    
    # change NAs to 0s
    #bird_df[is.na(bird_df)] <- 0
    
    # log counts for log scale in jags
    #bird_df <- mutate(bird_df, logCounts = log(bird_df[,1] + 1))
    #bird_df$logCounts[is.infinite(bird_df$logCounts)] <- 0
    
    # get summer and winter count lengths
    summer <- bird_df[which(bird_df$season == 'S'),]
    winter <- bird_df[which(bird_df$season == 'W'),]
    #slength <- nrow(summer)
    #wlength <- nrow(winter)
    
    # data list for jags analysis
    data_jags <- list(summer = summer$logCounts,
                      winter = winter$logCounts,
                      #slength = slength,
                      #wlength = wlength,
                      N = nrow(bird_df)/2)
    
    # variables to be tracked
    params <- c('mu_t', 'mu_wt', "lambda", 'beta', 'winter', 'summer')
    
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
  }
  
  return(jag.mod)
}


ts_jag_plot <- function(jag.model, bird_df, spec_type){
  
  # complete the data frame to include all years
  #bird_df$startDate <- year(bird_df$startDate)
  
  #bird_df <- bird_df[-which(bird_df$season == 'O'),]
  bird_df <- mutate(bird_df, logCounts = log(bird_df[,1] + 1))
  
  if(spec_type == 1){
    
    # removing all out of season counts
    #bird_df <- bird_df[-which(bird_df$season == 'O'),]
    
    # change NAs to 0s
    #bird_df[is.na(bird_df)] <- 0
    
    # log counts for log scale in jags
    #bird_df[,1] <- log(bird_df[,1])
    #bird_df[,1][is.infinite(bird_df[,1])] <- 0
    
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
      facet_wrap(~ season, ncol = 1, )
  }
  else if(spec_type == 2){
    
    # change NAs to 0s
    #bird_df[is.na(bird_df)] <- 0
    
    # log counts for log scale in jags
    #bird_df <- bird_df %>% mutate(logCounts = log(bird_df[,1] + 1))
    #bird_df[,1] <- log(bird_df[,1])
    #bird_df$logCounts[is.infinite(bird_df$logCounts)] <- 0
    
    #jag.summary <- summary(jag.model)
    #est.pop <- jag.summary[,"Median"]
    #lower95 <- jag.summary[,"Lower95"]
    #upper95 <- jag.summary[,"Upper95"]
    
    sEstimated <- jag.model$mean$mu_t
    sLower <- jag.model$q2.5$mu_t
    sUpper <- jag.model$q97.5$mu_t
    
    # separating the summer and winter counts
    summer <- bird_df[which(bird_df$season == 'S'),]
    winter <- bird_df[which(bird_df$season == 'W'),]
    #slength <- nrow(summer)
    #wlength <- nrow(winter)
    
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
    
    # if (slength < wlength) {
    #   winterdf <- data.frame(Year = winter$startDate, 
    #                          lambda = est.pop[(slength+((wlength-slength) + 1)):length(est.pop)],
    #                          s_estimated = est.pop[1:wlength],
    #                          w_counts = winter$logCounts,
    #                          lower = lower95[(slength+((wlength-slength) + 1)):length(est.pop)],
    #                          upper = upper95[(slength+((wlength-slength) + 1)):length(est.pop)])
    # }
    # else{
    #   winterdf <- data.frame(Year = winter$startDate, 
    #                          lambda = est.pop[(slength + 1):length(est.pop)],
    #                          s_estimated = est.pop[1:wlength],
    #                          w_counts = winter$logCounts,
    #                          lower = lower95[(slength + 1):length(est.pop)],
    #                          upper = upper95[(slength + 1):length(est.pop)])
    # }
    
    #winterdf <- winterdf %>% 
    #mutate(w_estimated = s_estimated + lambda)
    
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
    
      
      # scale_color_manual(name = "Legend",
      #                    breaks = c("State process (s)", "Counts(s)"),
      #                    values = c("State process (s)" = "grey1", "Counts(s)" = "red"))
      
      # labs(x = "Year",
      #      y = "Population") 
      
       # scale_color_identity(guide = "Legend",
       #                      name = "Legend",
       #                      labels = c("State process (s)", "Counts(s)")) 
      
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
      
      # labs(x = "Year",
      #      y = "Population") 
      
      # scale_color_identity(guide = "legend",
      #                      name = "Legend",
      #                      labels = c("State process (w)", "Counts (w)")) 
    
    #plot_grid(summer_plot, winter_plot, row = 2, ncol = 1)
    grid.arrange(summer_plot, winter_plot,
                 nrow = 2, heights = c(1/2, 1/2),
                 left = "Log Population",
                 bottom = "Year")
                 #top = plottitle)
    
  }
}

