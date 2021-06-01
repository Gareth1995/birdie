#' Calculate the index for a given population/group for each year
#'
#' @param indices numeric string of counts for a species
#'
#' @return a vector of index values for each year of a given species
#' @export

# calculate the index
calcIndex <- function(trends){
  
  indices <- c()
  trend.mean <- 10**mean(trends, na.rm = T)
  
  for(i in 1:length(trends)){
    if(i == 1){
      index <- 1 * trend.mean
      indices <- c(indices, index)
    }else{
      index <- indices[i-1] * trend.mean
      indices <- c(indices, index)
    }
  }
  
  return(indices)
}