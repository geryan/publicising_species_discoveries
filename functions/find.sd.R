find.sd <- function(
  lower,
  mid,
  upper,
  p,
  tolerance = 1e-7,
  print.sd = TRUE
){
  
  if(p == 1){
    return("triang")
  }
  
  
  if(lower == mid |
     (lower == 0 & p <= 0.5)){
    
    b_seq <- seq(
      from = upper,
      to = 10*upper,
      length.out = 1000
    )
    
    library(extraDistr)
    
    uppers <- qtriang(
      p = p,
      a = lower,
      b = b_seq,
      c = mid
    )
    
    diffs <- abs(upper - uppers)
    
    diff <- min(diffs)
    
    if(diff <= tolerance){
      return(bs[which.min(diffs)])
    }
    
    while(diff > tolerance){
      
      uppers <- qtriang(
        p = p,
        a = lower,
        b = b_seq,
        c = mid
      )
      
      diffs <- upper - uppers
      
      diffs.above <- diffs[which(diffs > 0)]
      diffs.below <- diffs[which(diffs < 0)]
      
      diff.above <- min(diffs.above)
      diff.below <- max(diffs.below)
      
      diff <- min(diff.above, abs(diff.below))
      
      index.above <- which(diffs == diff.above)
      index.below <- which(diffs == diff.below)
      
      b.above <- b_seq[index.above]
      b.below <- b_seq[index.below]
      
      b <- ifelse(
        diff == diff.above,
        b.above,
        b.below
      )
      
      
      b_seq <- seq(
        from = b.below,
        to = b.above,
        length.out = 1000
      )

      if(print.sd){
        print(b)
        print(diff)
      }
      
    }
    
    return(b)
  }
  

  
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
    
    diffs <- p - (probs_upper - probs_lower)
    
    diffs.above <- diffs[which(diffs > 0)]
    diffs.below <- diffs[which(diffs < 0)]
    
    diff.above <- min(diffs.above)
    diff.below <- max(diffs.below)
    
    diff <- min(diff.above, abs(diff.below))
    
    index.above <- which(diffs == diff.above)[1]
    index.below <- which(diffs == diff.below)[length(which(diffs == diff.below))]
    
    sd.above <- sd_seq[index.above]
    sd.below <- sd_seq[index.below]
    
    sd <- ifelse(
      diff == diff.above,
      sd.above,
      sd.below
    )
    
    sd_seq <- seq(
      from = sd.below,
      to = sd.above,
      length.out = 1000
    )
    
    if(print.sd){
      print(sd)
      print(diff)
    }
    
  }
  
  return(sd)
}
