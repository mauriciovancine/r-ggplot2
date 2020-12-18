### script pca ###

# Maurí­cio Humberto Vancine - mauricio.vancine@gmail.com

###-------------------------------------------------------------------------------------------###

# packages
library(ggplot2)
library(ellipse)
library(ggfortify)

###-------------------------------------------------------------------------------------------###

# data
ma <- matrix(rnorm(1200), 100, 12)
head(ma)

da <- data.frame(ma, fa = factor(rep(c("A", "B"), each = 50)))
head(da)

# pca
pca <- prcomp(da[, -13], scale = T)

summary(pca)

# plot
biplot(pca)


###-------------------------------------------------------------------------------------------###

# plot ggplot2
autoplot(pca)

autoplot(pca, scale = 0)

autoplot(pca, data = da, colour = "fa")

autoplot(pca, data = da, colour = "fa", label = T, label.size = 3)

autoplot(pca, data = da, colour = "fa", shape = F, label.size = 3)

autoplot(pca, data = da, colour = "fa", loadings = T)

autoplot(pca, data = da, colour = "fa", loadings = T, loadings.colour = 'blue',
         loadings.label = T, loadings.label.size = 4,
	   xlab = paste0("PC1: ", round(summary(pca)$importance[2, 1], digits = 2) * 100, "% variance"),
         ylab = paste0("PC2: ", round(summary(pca)$importance[2, 2], digits = 2) * 100, "% variance")) +
theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 15))

###-------------------------------------------------------------------------------------------###

# plot ellipse ggplot2
scores <- as.data.frame(pca$x)
scores

# define groups for plotting
scores$groups <- da$fa
head(scores)

# centroids  
centroids <- aggregate(cbind(PC1, PC2) ~ groups, scores, mean)
centroids

# ellipse data
conf.rgn <- do.call(rbind, lapply(unique(scores$groups), function(t)
  data.frame(groups = as.character(t),
             ellipse(cov(scores[scores$groups == t, 1:2]),
                   centre = as.matrix(centroids[centroids$groups == t, 2:3]),
                   level = 0.95),
             stringsAsFactors = FALSE)))

# plot ellipse    
ggplot(data = scores, aes(x = PC1, y = PC2, group = groups, color = groups)) + 
    geom_polygon(data = conf.rgn, aes(fill = groups), alpha = 0.2) +
    geom_point(size = 2, alpha = 0.6) + 
    scale_color_brewer(palette = "Set1") +
    theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size = 15)) +
    labs(color = "",
         fill = "",
         x = paste0("PC1: ", round(summary(pca)$importance[2, 1], digits = 2) * 100, "% variance"),
         y = paste0("PC2: ", round(summary(pca)$importance[2, 2], digits = 2) * 100, "% variance")) 

###-------------------------------------------------------------------------------------------###





