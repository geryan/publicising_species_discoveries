cumulative.density <- function(
  species,
  value,
  data,
  nsamples,
  decision
){
  
  dn <- decision
  sp <- species
  
  cd <- data %>%
    filter(species == sp) %>%
    filter(decision == dn) %>%
    mutate(
      cn = ifelse(
        test = zeta <= value,
        yes = 1,
        no = 0
      )
    ) %>%
    summarise(
      cn = sum(cn)
    ) %>%
    unlist %>%
    divide_by(nsamples)
  
  return(cd[1])
}