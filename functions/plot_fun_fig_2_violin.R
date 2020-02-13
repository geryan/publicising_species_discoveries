plot_fun_fig_2_violin <- function(
  species,
  data,
  limits = NULL,
  xlab = TRUE,
  ylab = TRUE,
  textsize = 9
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
        direction = "vertical",
        title.theme = element_text(
          size = textsize
        )
      ) 
    ) +
    theme(
      # axis.text.x = element_text(
      #   angle = 270
      # ),
      axis.text.x = element_blank(),
      axis.title.x = element_text(
        size = textsize
      ),
      axis.title.y = element_text(
        size = textsize
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
      ylab("Decision score")
  }
  
  return(result)
  
  
}