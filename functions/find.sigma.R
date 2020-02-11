find.sigma <- function(
  lower,
  most.likely,
  upper,
  p,
  tolerance = 1e-7,
  print.sigma = FALSE
){
  
  sigma <- 1
  
  mu <- log(most.likely) + sigma^2
  
  probs1 <- plnorm(
    q = c(lower, upper),
    meanlog = mu,
    sdlog = sigma
  )
  
  prob.width <- probs1[2] - probs1[1]
  
  diff <- abs(p - prob.width)
  
  if(diff <= tolerance){
    return(1)
  }
  
  if(prob.width > p){
    sigma_seq <- seq(
      from = 1,
      to = 10e6,
      length.out = 1000
    )
  } else{
    sigma_seq <- seq(
      from = 0,
      to = 1,
      length.out = 1000
    )
  }
  
  mu_seq <- log(most.likely) + sigma_seq^2
  
  while(diff > tolerance){
    probs_upper <- plnorm(
      q = upper,
      meanlog = mu_seq,
      sdlog = sigma_seq
    )
    
    probs_lower <- plnorm(
      q = lower,
      meanlog = mu_seq,
      sdlog = sigma_seq
    )
    
    diffs <- p - (probs_upper - probs_lower)
    
    if(!between(
      x = 0,
      left = range(diffs)[1],
      right = range(diffs)[2]
    )){
      return(NA)
    }
    
    diffs.above <- diffs[which(diffs > 0)]
    diffs.below <- diffs[which(diffs < 0)]
    
    diff.above <- min(diffs.above)
    diff.below <- max(diffs.below)
    
    diff <- min(diff.above, abs(diff.below))
    
    index.above <- which(diffs == diff.above)[1]
    index.below <- which(diffs == diff.below)[length(which(diffs == diff.below))]
    
    sigma.above <- sigma_seq[index.above]
    sigma.below <- sigma_seq[index.below]
    
    sigma <- ifelse(
      diff == diff.above,
      sigma.above,
      sigma.below
    )
    
    mu.above <- mu_seq[index.above]
    mu.below <- mu_seq[index.below]
    
    mu <- ifelse(
      diff == diff.above,
      mu.above,
      mu.below
    )
    
    
    sigma_seq <- seq(
      from = sigma.below,
      to = sigma.above,
      length.out = 1000
    )
    
    mu_seq <- log(most.likely) + (sigma_seq)^2
    
    if(print.sigma){
      print(sigma)
      print(mu)
      print(diff)
    }
    
  }
  
  return(sigma)
}
