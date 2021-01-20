level_order <- unique(finalCostsCI$provs)
costPlot <- ggplot( finalCostsCI, aes(
                                      x=factor(provs, level = level_order), 
                                      y=mean, ymin=lower, ymax=upper, 
                                      group=type, color = factor(type)
                                      ) 
                    ) +
  theme_bw() +
  geom_point() +
  geom_smooth(stat="identity") +
  theme(legend.position = "right", panel.grid.minor.x = element_blank()) +
  xlab("Number of Providers") + ylab("Average slice cost") +
  scale_color_discrete("Type:", labels = c("opt", "first", "random")) +
  facet_wrap(~demand, scales = "free_y")