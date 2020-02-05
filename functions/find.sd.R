find.sd <- function(
  lower,
  mid,
  upper,
  p,
  tolerance = 1e-7,
  print.sd = TRUE
){
  
  ll <- log(lower)
  lm <- log(mid)
  lu <- log(upper)
  
  probs1 <- pnorm(
    q = c(ll, lu),
    mean = lm,
    sd = 1
  )
  
  prob.width <- probs1[2] - probs1[1]
  
  diff <- abs(p - prob.width)
  
  if(diff <= tolerance){
    return(1)
  }
  
  if(prob.width > p){
    sd_seq <- seq(
      from = 1,
      to = 10e6,
      length.out = 1000
    )
  } else{
    sd_seq <- seq(
      from = 0,
      to = 1,
      length.out = 1000
    )
  }
  
  while(diff > tolerance){
    probs_upper <- pnorm(
      q = lu,
      mean = lm,
      sd = sd_seq
    )
    
    probs_lower <- pnorm(
      q = ll,
      mean = lm,
      sd = sd_seq
    )
    
    diffs <- abs(p - (probs_upper - probs_lower))
    
    index.nearest <- which.min(diffs)
    
    diff <- diffs[index.nearest]
    
    sd <- sd_seq[index.nearest]
    
    diffs2 <- diffs[-index.nearest]
    
    index2 <- which.min(diffs2)
    
    diff2 <- diffs2[index2]
    
    indexsd2 <- which(diffs == diff2)
    
    sd2 <- sd_seq[indexsd2]
    
    sd_seq <- seq(
      from = min(sd, sd2),
      to = max(sd, sd2),
      length.out = 1000
    )
    
    if(print.sd){
      print(sd)
      print(diff)
    }
    
  }
  
  return(sd)
}
