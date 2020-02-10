find.b


if(p == 1){
  return(NA)
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