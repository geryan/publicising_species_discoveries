sample.fun <- function(
  n,
  dist,
  most.likely,
  value
){
  
  if(dist == "triangular"){
    
    library(extraDistr)
    
    result <- rtriang(
      n = n,
      a = 0,
      b = value,
      c = most.likely
    )
    
    return(result)
    
  } else if(dist == "exponential"){
    
    result <- rexp(
      n = n,
      rate = value
    )
    
    return(result)
    
  } else if(dist == "lognormal"){
    
    result <- rlnorm(
      n = n,
      meanlog = log(most.likely) + value^2,
      sdlog = value
    ) 
    
    return(result)
    
  }
}