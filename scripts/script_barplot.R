### scripts ggplot2 ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###
### barplot ###
###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2)

###---------------------------------------------------------------------###

# data
da <- data.table(x = sample(1:100, 10), y = sample(LETTERS, 10))

# plot
ggplot(da, aes(x = reorder(y, -x), y = x)) +
  geom_bar(colour = "gray5", fill = "gray50", stat = "identity") +
  labs(x = NULL, y = NULL) +
  coord_flip() + 
  theme_classic()

# export
ggsave("barplot.tif", dpi = 300)

###---------------------------------------------------------------------###

