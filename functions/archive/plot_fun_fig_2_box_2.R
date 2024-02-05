plot_fun_fig_2_box_2 <- function(
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
    geom_boxplot(
      aes(
        x = decision,
        y = zeta,
        fill = decision
      ),
      outlier.size = 0.5
    ) + 
    geom_hline(
      yintercept = 0,
      colour = "darkgrey",
      size = 1
    ) +
    scale_fill_manual(
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
      facet_zoom(
        ylim = c(limlow, limupp),
        zoom.size = 1
      )
  }
  
  
  if(xlab){
    result <- result +
      xlab("Decision to disclose") +
      theme(
        axis.title.x = element_text(
          size = textsize,
          hjust = 1
        )
      )
  }
  
  if(ylab){
    result <- result +
      ylab("Decision\nscore") +
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
          size = 10
        )
      )
  }
  
  return(result)
  
  
}