costPlot <- ggplot( totalCosts, aes(x=provs, y=mean, ymin=lower, ymax=upper,
                                    group=type, color = factor(-type)) ) +
  theme_bw() +
  geom_point() +
  geom_smooth(stat="identity") +
  theme(legend.position = "right", panel.grid.minor.x = element_blank()) +
  xlab("Number of Providers") + ylab("Average slice cost") +
  scale_x_continuous(breaks=seq(5, 20, 1)) +
  scale_color_discrete("Legend:", breaks = unique(factor(-totalCosts$type)),
                       labels = c("opt", "first", "random")) +
  facet_wrap(~demand, scales = "free_y")