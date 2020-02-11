find.b <- function(
  lower,
  most.likely,
  upper,
  p,
  tolerance = 1e-7,
  print.b = FALSE
){
  
  if(p == 1){
    return(upper)
  }
  
  
  library(extraDistr)
  
  b_seq <- seq(
    from = upper,
    to = 100*upper,
    length.out = 1000
  )
  
  diff <- tolerance + 1
  
  while(diff > tolerance){
    
    prob_upper <- ptriang(
      q = upper,
      a = 0,
      b = b_seq,
      c = most.likely
    )
    
    prob_lower <- ptriang(
      q = lower,
      a = 0,
      b = b_seq,
      c = most.likely
    )
    
    
    diffs <- p - (prob_upper - prob_lower)
    
    diff <- min(diffs)
    
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
    
    if(print.b){
      print(b)
      print(diff)
    }
    
  }
  
  return(b)
  
}