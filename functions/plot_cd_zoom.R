plot_cd_zoom <- function(
  species,
  data,
  limits = NULL,
  xlab = TRUE,
  ylab = TRUE,
  textsize = 9,
  xtext = TRUE
){
  
  library(ggplot2)
  library(dplyr)
  
  sp <- species
  
  x <- data %>%
    filter(species == sp)
  
  
  result <- ggplot(x) + 
    geom_vline(
      xintercept = 0,
      colour = "darkgrey",
      size = 0.8
    ) +
    geom_line(
      aes(
        x = value,
        y = cd,
        colour = decision
      ),
      size = 0.8
    ) + 
    scale_colour_manual(
      values = viridis(3)[3:1],
      guide = guide_legend(
        title = "Decision\nto disclose",
        direction = "horizontal",
        title.theme = element_text(
          size = textsize
        )
      )
      #guide = FALSE
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        size = 10
      ),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    #facet_wrap(~species) +
    xlab("") +
    ylab("") + 
    expand_limits(y = 0)
  
  
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
      xlim(limlow, limupp)
  }
  
  
  if(xlab){
    result <- result +
      xlab("Decision score") +
      theme(
        axis.title.x = element_text(
          size = textsize,
          hjust = 1
        )
      )
  }
  
  if(ylab){
    result <- result +
      ylab("Cumulative\n density") +
      theme(
        axis.title.y = element_text(
          size = textsize
        )
      )
  }
  
  if(xtext) {
    result <- result  +
      theme(
        axis.text.x = element_text(
          angle = 270,
          size = 7
        )
      )
  }
  
  return(result)
  
  
}