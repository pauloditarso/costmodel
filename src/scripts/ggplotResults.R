# Packages needed
packages_needed <- c("ggplot2")

# Install packages not yet installed
packages_installed <- packages_needed %in% rownames(installed.packages())
if (any(packages_installed == FALSE)) {
  print(paste(packages_needed[!packages_installed], "not installed!!", sep = " "))
  quit()
}

packages_loaded <- packages_needed %in% .packages()
if (any(packages_loaded == FALSE)) {
  invisible(lapply(packages_needed[!packages_loaded], library, character.only = TRUE))
  rm(packages_installed, packages_loaded, packages_needed)
}

rm(packages_installed, packages_loaded, packages_needed)

level_order <- unique(allCostsCI$provs)

costPlot <- ggplot( allCostsCI, aes(
                      x=factor(provs, level = level_order),
                      y=as.numeric(mean), ymin=as.numeric(lower), ymax=as.numeric(upper), 
                      group=type, color = factor(type)
                                    ) 
                    ) +
  theme_bw() +
  geom_point() +
  geom_smooth(stat="identity") +
  theme(legend.position = "top", panel.grid.minor.x = element_blank()) +
  xlab("Number of providers") + ylab("Average slice cost") +
  scale_color_discrete("Type:", labels = c("optimized", "first choice", "random choice")) +
  facet_wrap(~demand, scales = "free")

repsPlot <- ggplot( allRepsCI, aes(
                      x=factor(provs, level = level_order),
                      y=as.numeric(mean), ymin=as.numeric(lower), ymax=as.numeric(upper), 
                      group=type, color = factor(type)
                                    ) 
                    ) +
  theme_bw() +
  geom_point() +
  geom_smooth(stat="identity") +
  theme(legend.position = "top", panel.grid.minor.x = element_blank()) +
  xlab("Number of providers") + ylab("Average percentage of used providers") +
  scale_color_discrete("Type:", labels = c("optimized", "10% discounted", "20% discounted", "30% discounted")) +
  facet_wrap(~demand, scale = "free_y")

discPlot <- ggplot( allDiscCI, aes(
                    x=factor(provs, level = level_order),
                    y=as.numeric(mean), ymin=as.numeric(lower), ymax=as.numeric(upper),
                    group=type, color = factor(type)
                                ) 
                  ) +
  theme_bw() +
  geom_point() +
  geom_smooth(stat="identity") +
  theme(legend.position = "top", panel.grid.minor.x = element_blank()) +
  xlab("Number of providers") + ylab("Average discount rate") +
  scale_color_manual(values = c("#CC79A7", "#E7B800", "#52854C"), "Type:", labels = c("10% discounted", "20% discounted", "30% discounted")) +
  facet_wrap(~demand)