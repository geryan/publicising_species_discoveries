s1_plot_fun <- function(x){
  
  result <- ggplot(
    data = numeric_results %>%
    filter(species == x)
  ) +
    # geom_hline(
    #   yintercept = 0,
    #   colour = "darkgrey"
    # ) +
    geom_point(
      aes(
        x = decision,
        y = zeta,
        colour = risk_scenario,
        shape = optimal,
        size = optimal
      )
    ) +
    facet_wrap(
      facets = ~ risk_scenario,
      nrow = 1,
      scales = "free_y"
    ) +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_text(
        angle = 0
      ),
      legend.position = "bottom",
      legend.justification = "left"
    ) +
    ylab("Decision\nscore") +
    xlab("Decision to disclose") +
    scale_colour_manual(
      values = viridis(3),
      guide = guide_legend(
        title = "Risk\nscenario",
        direction = "horizontal"
      ) 
    ) +
    scale_shape_manual(
      values = c(19, 17),
      guide = guide_legend(
        title = "Decision is\noptimal?",
        direction = "horizontal"
      )
    ) +
    scale_size_manual(
      values = c(4, 5),
      guide = guide_legend(
        title = "Decision is\noptimal?",
        direction = "horizontal"
      )
    )
  
  return(result)
}