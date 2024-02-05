find.lambda <- function(
  upper,
  p,
  lower = 0,
  tolerance = 1e-7,
  print.lambda = FALSE
){
  
  lambda_seq <- seq(
    from = 0,
    to = 1,
    length.out = 1000
  )

  diff <- tolerance + 1
  
  
  while(diff > tolerance){
    probs_upper <- pexp(
      q = upper,
      rate = lambda_seq
    )
    
    probs_lower <- pexp(
      q = lower,
      rate = lambda_seq
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
    
    lambda.above <- lambda_seq[index.above]
    lambda.below <- lambda_seq[index.below]
    
    lambda <- ifelse(
      diff == diff.above,
      lambda.above,
      lambda.below
    )

    
    lambda_seq <- seq(
      from = lambda.below,
      to = lambda.above,
      length.out = 1000
    )
    
    if(print.lambda){
      print(lambda)
      print(diff)
    }
    
  }
  
  return(lambda)
  
}