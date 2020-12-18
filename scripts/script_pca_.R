

setwd("C:/Users/Thadeu Sobral/Desktop/")

dados<-read.table("species2.txt",header=T) #leitura dos dados
names(dados)
str(dados)
attach(dados)
dados
newdata <- na.omit(dados)
# Load data
head(newdata, 124)
# log transform
log.ir <- log(newdata[,1:6])
ir.species <- newdata[, 7]

# apply PCA - scale. = TRUE is highly
# advisable, but default is FALSE.
ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
# print method
print(ir.pca)
# plot method
plot(ir.pca, type = "l")
# summary method
summary(ir.pca)
# Predict PCs
 predict(ir.pca,newdata=tail(log.ir, 2))

install.packages("devtools")
install.packages("ggplot2")
install.packages('ggfortify')
library(devtools)
library(ggplot2)
library(ggfortify)


install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
              groups = ir.species, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)
