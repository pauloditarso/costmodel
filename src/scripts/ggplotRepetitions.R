repPlot <- ggplot( finalRepsCI, aes(x=as.numeric(provs), 
                                    y=as.numeric(mean), fill=type) ) +
  theme_bw() +
  geom_bar(stat="identity", position = "dodge", width=.5) +
  geom_errorbar(aes(ymin=as.numeric(lower), ymax=as.numeric(upper)),
                position = "dodge", width=.5) +
  # theme(legend.position = "right", panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks=5:10) +
  scale_y_continuous(breaks=seq(from = 0, to = 1, by = 0.1), limits = c(0,1)) +
  labs(x = "Number of providers", y = "Fraction of used providers (%)") +
  scale_fill_discrete(name = "Type:", labels = c("Opt", "10%", "20%", "30%"))