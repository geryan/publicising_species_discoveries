plot_fun_fig_1_violin <- function(
  species,
  data,
  limits = NULL,
  xlab = TRUE,
  ylab = TRUE
){
  
  library(ggplot2)
  library(dplyr)
  
  sp <- species
  
  x <- data %>%
    filter(species == sp)
  
  
  
  result <- ggplot(x) +
    geom_violin(
      aes(
        x = decision,
        y = zeta,
        fill = decision
      )#,
      #draw_quantiles = c(0.25, 0.5, 0.75)
    ) + 
    geom_hline(
      yintercept = 0,
      colour = "darkgrey"
    ) +
    scale_fill_manual(
      values = viridis(3)[3:1],
      guide = guide_legend(
        title = "Decision\nto disclose",
        direction = "vertical"
      ) 
    ) +
    theme(
      axis.text.x = element_text(
        angle = 270
      ),
      strip.text = element_text(
        
      )
    ) +
    facet_wrap(~species) +
    xlab("") +
    ylab("")
  
  
  if(!is.null(limits)){
    limlow <- limits %>%
      filter(species == sp) %>%
      select(lower) %>%
      unlist
    
    limupp <- limits %>%
      filter(species == sp) %>%
      select(upper) %>%
      unlist
    
    limlow <- ifelse(limlow > 0, 0, limlow)
    limupp <- ifelse(limupp < 0, 0, limupp)
    
    result <- result + 
      ylim(limlow, limupp)
  }
  
  
  if(xlab){
    result <- result +
      xlab("Decision to disclose")
  }
  
  if(ylab){
    result <- result +
      ylab("Decision\nscore")
  }
  
  return(result)
  
  
}