# biplot three axes

# packages
library(tidyverse)
library(metR)
library(raster)
library(magick)

# image
download.file(url = "http://phylopic.org/assets/images/submissions/3bbd487e-31ef-4121-b030-f06f830132b8.512.png",
              destfile = "anuran.png", mode = "wb")
img <- magick::image_read("anuran.png")
g <- grid::rasterGrob(img, interpolate = TRUE)

# model
set.seed(42)
da <- tibble::tibble(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y =  rbinom(100, 1, .5)
)
da

model <- glm(y ~ x1 + x2, data = da, family = "binomial")
summary(model)

# predict
m <- round(raster::predict(model), 3)
head(m)

x <- model$data$x1
y <- model$data$x2
d <- as.data.frame(cbind(m, x, y))

f <- seq(range(d$x)[1],range(d$x)[2], length=100)
s <- seq(range(d$y)[1],range(d$y)[2], length=100)
fs <- expand.grid(f, s)
colnames(fs) <- c("x1", "x2")
head(fs)

p <- data.frame(predict(model, newdata = fs, type = "response"))
fs <- cbind(fs, p)
colnames(fs) <- c("x1", "x2", "p")
head(fs)

na <- 1:(10000-length(x))
na[na > 0] <- NA
xna <- c(x, na)

na <- 1:(10000-length(y))
na[na > 0] <- NA
yna <- c(y, na)

# plot
ggplot(data = fs, aes(x = fs$x1, y = fs$x2, z = fs$p))+
  geom_raster(aes(fill = fs$p), show.legend = TRUE) +
  scale_fill_distiller(palette = "RdBu") +
  geom_contour(aes(z = fs$p), color = "black", size = .9, alpha = .5)+
  geom_text_contour(vjust = 1.2, size = 10, stroke = .05)+
  geom_point(aes(x = xna, y = yna), shape = 16, size = 6, alpha = .3) +
  labs(x = "X1", y = "X2", fill = "Probability") +
  theme_classic() +
  theme(axis.text = element_text(size = 22, colour = "black"),
        axis.title = element_text(size = 28),
        legend.title = element_text(size = 22),
        legend.text = element_text(size = 20),
        legend.key.size = unit(.7, "cm"),
        legend.key.width = unit(.7,"cm")) +
  annotation_custom(grob = g, xmin = -3, xmax = -2, ymin = 1, ymax = 2)

ggsave(filename = "plot.png", width = 30, height = 20, units = "cm", dpi = 300)
