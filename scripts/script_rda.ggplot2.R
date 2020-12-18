### scripts rda ###

# Maurí­cio Humberto Vancine - mauricio.vancine@gmail.com

###-------------------------------------------------------------------------------------------###

# packages
library(vegan)
library(ggplot2)
library(grid)

###-------------------------------------------------------------------------------------------###

# data
da.e <- data.frame(va1 = rnorm(100, 2), va2 = rnorm(100, 5), va3 = rnorm(100))
da.e

da.s <- data.frame(sp1 = rpois(100, 1), sp2 = rpois(100, 2), sp3 = rpois(100, 2), 
			 sp4 = rpois(100, 2), sp5 = rpois(100, 1), sp6 = rpois(100, 2))
da.s

# rda
rda <- rda(da.s, decostand(da.e, "stand"))
rda

summary(rda)
anova(rda)

R2 <- RsquareAdj(rda)$r.squared
R2

anova.cca(rda, step = 1000) # todos os eixos

# plot
plot(rda, scaling = 3, display = c("sp", "lc", "cn"))


# ggplot
smry <- summary(rda, scaling = 3)

df1 <- data.frame(smry$sites)
df2 <- data.frame(smry$species) 
df3 <- data.frame(smry$biplot) 

rda.plot1 <- ggplot(df2, aes(x = RDA1, y = RDA2)) + 
 geom_text(aes(label = rownames(df2)), size = 5, color = "gray20", fontface = 3) +
 geom_hline(yintercept = 0, linetype = "dotted", size = 1) +
 geom_vline(xintercept = 0, linetype = "dotted", size = 1) +
 theme(axis.text = element_text(size = 18, colour = "black"), axis.title = element_text(size = 18)) +
 xlab("RDA1") +
 ylab("RDA2") +
 theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
  panel.background = element_blank(), axis.line = element_line(size = 1))
rda.plot1


rda.biplot <- rda.plot1 +
 geom_segment(data = df3, aes(x = 0, xend = RDA1, y = 0, yend = RDA2), 
    color = "black", size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
 geom_text(data = df3, 
   aes(x = RDA1, y = RDA2, label = rownames(df3), 
   hjust = 0.1*(1-sign(RDA1)), vjust = 0.5*(1-sign(RDA2))), 
   size = 7, color = "black")#+
#geom_text(data = df1, aes(x = RDA1, y = RDA2, label = rownames(df1)), size = 3, color = "gray55", fontface = 1)
rda.biplot

ggsave("rda.tiff", wi = 18, he = 18, un = "cm")




