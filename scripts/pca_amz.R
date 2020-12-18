library (stats)
library (ggbiplot)
library(vegan)
library(devtools)
install_github("ggbiplot", "vqv")
setwd("/media/carlos/DATA/Dadosdoprojeto/Amazonia/Tabela e resultados finais")
# Tabela de atributo completa
atri_full1 <- read.table("Classes_area_atributos_AMZ_semareassemdados_SD.csv", header=T, sep=",", dec=".")
head(atri_full1)
library (dplyr)
atri_full1 = atri_full1[atri_full1$Final > 1,]
head(atri_full1)
#atri_full <- atri_full1[-1,]
atri_stats <-atri_full1[,12:20]
head(atri_stats)
classes <- as.factor (atri_full1[,11])
head(classes)
# Normalizaçao das variaveis
atri_norm <- decostand(atri_stats, method = "normalize", na.rm = TRUE, MARGIN=2)
head(atri_norm)
ir.pca <- prcomp(atri_norm,
                 center = TRUE,
                 scale. = TRUE) 


# print method
print(ir.pca)
# plot method
plot(ir.pca, type = "l")
# summary method
summary(ir.pca)
# Predict PCs

predict(ir.pca, 
        newdata=tail(atri_norm, 2))

g <- ggbiplot(ir.pca, obs.scale = 2, var.scale = 0.9, 
              groups = classes, ellipse = F, 
              circle = F, alpha = F, var.axes = T, varname.size=4,varname.adjust=3, arrow.color = "blue")+theme_classic()
g <- g + scale_color_manual(name="Class", values=c("#732600", "#000000", "#FF5500", "#FFAA00", "#E60000", "#D7D79E", "#CDAA66","#267300", "#A8A800", "#734C00", "#728944", "#FFD37F", "#E6E600"))
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top') + ylim(c(-12, 18))
print(g)
ggsave("gtSD.tiff",wi =18, he=18, un="cm", dpi=300)
g <- plot3d(ir.pca$x[,1:3], col=atri_full1$Final, type='s', size=1)
##################################Mean
atri_full1 <- read.table("Classes_area_atributos_AMZ_semareassemdados_Mean.csv", header=T, sep=",", dec=".")
head(atri_full1)
library (dplyr)
atri_full1 = atri_full1[atri_full1$Final > 1,]
head(atri_full1)
#atri_full <- atri_full1[-1,]
atri_stats <-atri_full1[,12:20]
head(atri_stats)
classes <- as.factor (atri_full1[,11])
head(classes)
# Normalizaçao das variaveis
atri_norm <- decostand(atri_stats, method = "normalize", na.rm = TRUE, MARGIN=2)
head(atri_norm)
ir.pca <- prcomp(atri_norm,
                 center = TRUE,
                 scale. = TRUE) 


# print method
print(ir.pca)
# plot method
plot(ir.pca, type = "l")
# summary method
summary(ir.pca)
# Predict PCs

predict(ir.pca, 
        newdata=tail(atri_norm, 2))

g <- ggbiplot(ir.pca, obs.scale = 0.9, var.scale = 0.6, 
              groups = classes, ellipse = TRUE, 
              circle = TRUE, alpha = 0.06)
g <- g + scale_color_manual(name="Class", values=c("#732600", "#000000", "#FF5500", "#FFAA00", "#E60000", "#D7D79E", "#CDAA66","#267300", "#A8A800", "#734C00", "#728944", "#FFD37F", "#E6E600"))
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top') + ylim(c(-5, 7))
print(g)



library(scatterplot3d)
pairs(ir.pca$scores[,1:5], col=rainbow(13)[atri_full1[,11]], asp=1)

scatterplot3d(pc$scores[,c(1,2,4)], color=rainbow(3)[winedata[,1]])
