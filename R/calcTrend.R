#' Calculate the rate of change for a given population for each year
#'
#' @param counts numeric string of counts for a species
#'
#' @return a vector of rate of change values for each year
#' @export

# creating a function to calculate trends for individual species
calcTrend <- function(counts){
  
  trends <- c()
  
  for(i in 2:length(counts)){
    trend <- log10(counts[i]/counts[i-1])
    trends <- c(trends, trend)
  }
  return(trends)
}