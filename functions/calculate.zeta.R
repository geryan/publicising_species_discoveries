calculate.zeta <- function(Bd, Cd, pd, Cp){
  
  zeta <- Bd - Cd - pd*(Cp - Cd)
  
  return(zeta)
  
}