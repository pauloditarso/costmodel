repPlot <- ggplot( finalRepsCI, aes(x=as.numeric(provs), y=as.numeric(mean)) ) +
  theme_bw() +
  geom_bar(stat="identity", width=.5) +
  geom_errorbar(aes(ymin=as.numeric(lower), ymax=as.numeric(upper)), width=.2) +
  # theme(legend.position = "right", panel.grid.minor.x = element_blank()) +
  xlab("Number of Providers") + ylab("Fraction of Used Providers") +
  scale_x_continuous(breaks=5:10) +
  scale_y_continuous(breaks=seq(from = 0, to = 1, by = 0.1), limits = c(0,1))