### script r-plot ###

# mauricio vancine
# 05-04-2019


# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(ggpubr)
library(tidyverse)
library(wesanderson)

# data --------------------------------------------------------------------
da <- tibble::tibble(
  class = c("A", "B", "C", "D"),
  n = c(5, 2, 7, 8)
)
da

da <- da %>%
  mutate(prop = round(n/sum(n) * 100, 1)) %>% 
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5 * prop)
da


# ggplot2 -----------------------------------------------------------------
# pie
ggplot(da, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 4, "continuous")) +
  theme_void()

# donut
ggplot(da, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = wesanderson::wes_palette("Zissou1", 4, "continuous")) +
  theme_void()+
  xlim(0.5, 2.5)


# ggpubr ------------------------------------------------------------------
# simple
ggdonutchart(da, "prop", label = "class")

# color
ggdonutchart(da, "prop", label = "class",
             fill = "class", color = "white",
             palette = wesanderson::wes_palette("Zissou1", 4, "continuous"))

# labs
labs <- paste0(da$class, " (", da$prop, "%)")
ggdonutchart(da, "prop", label = labs,
             fill = "class", color = "white",
             palette = wesanderson::wes_palette("Zissou1", 4, "continuous"))

# labs inside
ggdonutchart(da, "prop", label = labs,
             lab.pos = "in", lab.font = "white",
             fill = "class", color = "white",
             palette = wesanderson::wes_palette("Zissou1", 4, "continuous"))

# end ---------------------------------------------------------------------
