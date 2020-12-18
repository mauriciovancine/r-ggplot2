### script ggpairs ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com
# 15/09/2017

###---------------------------------------------------------------------------###

## memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13) 

## packages
pacman::p_load(ggplot2, GGally)

###---------------------------------------------------------------------------###

# data
data(iris)

# plot
ggpairs(iris[, 1:4], 
        lower = list(continuous = wrap(ggally_smooth, color = "gray20", alpha = .2)),
        diag = list(continuous = wrap(ggally_barDiag, color = "gray50")),
        upper = list(continuous = wrap(ggally_cor, color = "black"))) +
  
  theme(text = element_text(size = 8, colour = "black"),
        axis.text = element_text(size = 5, colour = "black"), 
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10), 
        panel.grid.major = element_line(colour = "white"))

###---------------------------------------------------------------------------###
