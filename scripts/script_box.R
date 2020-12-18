ggplot(data = dat, aes(x = alg, y = TSS)) + 
    geom_boxplot() + 
    geom_jitter(colour = dat$col, width = 0.2) +
    theme(legend.position = "none") +
    xlab("Algorithms") +
    ylab("TSS") + 
    geom_hline(yintercept = .4, color = "red") + 
    ggtitle(g) + 
    theme(plot.title = element_text(lineheight = .8, face = "bold"), 
          axis.text = element_text(size = 12, colour = "black"), 
          axis.title = element_text(size = 15))

  ggsave(paste0("boxplot_jitter_", g, ".tiff"), he = 18, wi = 18, un = "cm", dpi = 300)}
  
