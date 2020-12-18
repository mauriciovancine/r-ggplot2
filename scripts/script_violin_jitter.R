### scripts ggplot2 - violin ###

###---------------------------------------------------------------------###

# Rafaela Silva	- rafaaps832@gmail.com
# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###

# memory
rm(list = ls())
gc()
memory.limit(size = 1.75e13)

# packages 
if(!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2)

###---------------------------------------------------------------------###

# import data
# da <- fread("E:/.csv")
# da

# data
da <- data.table(var = c(rnorm(50), rnorm(50, 2, 1)), 
                 cat = factor(rep(c("A", "B"), each = 50)))

# plot
ggplot(da, aes(x = cat, y = var, fill = cat)) + 

  geom_violin() + 
  stat_summary(fun.y = median, geom = "point", size = 2, color = "white") +
  geom_jitter(alpha = 0.5, position = position_jitter(width = 0.3)) + 
  
  labs(x = "cat", y = "var") +
  
  theme_classic() + 
  
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15)) +
  
  scale_fill_manual(values = c("orange", "blue"))

# export
ggsave("violin.tiff", wi = 15, he = 15, un = "cm", dpi = 300)  

###---------------------------------------------------------------------###




