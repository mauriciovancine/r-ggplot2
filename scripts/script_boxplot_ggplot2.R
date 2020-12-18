### scripts ggplot2 ###

# Mauricio Humberto Vancine - mauricio.vancine@gmail.com

###---------------------------------------------------------------------###
### boxplot ###
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
da <- data.frame(sui = runif(100, 0, 1), lan = factor(sample(letters[1:5], 100, rep = T)), 
		     col = NA)
str(da)
da

color <- c("red", "forestgreen", "blue", "orange", "gray")

for(i in 1:length(levels(da$lan))){
  da$col[da$lan == levels(da$lan)[i]] <- color[i]}

da


###-----------------------------------------------------------------------------------------###
				### boxplot ####
###-----------------------------------------------------------------------------------------###

# plot
ggplot(data = da, aes(x = lan, y = sui)) + 
	 geom_boxplot(fill = "white", colour = c("red", "forestgreen", "blue", "orange", "gray")) + 
	 theme(legend.position = "none") +
	 xlab("Classes") +
	 ylab("Variable")


# save
ggsave("boxplot.tiff", he = 18, wi = 18, un = "cm", dpi = 300)



###-----------------------------------------------------------------------------------------###
				### boxplot jitter ####
###-----------------------------------------------------------------------------------------###

# plot
ggplot(data = da, aes(x = lan, y = sui)) + 
	 geom_boxplot() + 
	 geom_jitter(colour = da$col, width = 0.2) +
	 theme(legend.position = "none") +
	 xlab("Classes") +
	 ylab("Variable")


# save
ggsave("boxplot_ jitter.tiff", he = 18, wi = 18, un = "cm", dpi = 300)

###-----------------------------------------------------------------------------------------###


###-----------------------------------------------------------------------------------------###
				### boxplot ####
###-----------------------------------------------------------------------------------------###


da <- data.frame(y = abs(rnorm(100)), x = factor(rep(c("Inside", "Outside"), each = 50)), 
			cl = NA, col = NA)
da

for(i in 1:nrow(da)){
  if(da[i, 1] <= .25){
    da[i, 3] <- "0.00-0.25"} 
  else if(da[i, 1] > .25 & da[i, 1] <= .5){  
    da[i, 3] <- "0.25-0.50"} 
  else if(da[i, 1] > .5 & da[i, 1] <= .75){  
    da[i, 3] <- "0.50-0.75"} 
  else if(da[i, 1] > .75){  
    da[i, 3] <- "0.75-1.00"}}

color <- c("red", "blue")

for(i in 1:length(levels(da$x))){
  da$col[da$x == levels(da$x)[i]] <- color[i]}


ggplot(data = da, aes(x = cl, y = y, fill = x)) + 
	 geom_boxplot() + guides(fill = F) +
 	 scale_fill_manual(values = c("blue", "red")) + 
 	 scale_colour_manual(values = c("blue", "red")) +
	 theme(legend.position = "none") +
       facet_wrap( ~x, scales = "fixed") +
	 xlab("Places") +
	 ylab("Suitability")

ggplot(data = da, aes(x = cl, y = y, fill = x)) + 
	 geom_boxplot() + guides(fill = F) +
 	 scale_fill_manual(values = c("white", "white")) + 
	 geom_jitter(colour = adjustcolor(da$col, .4), width = 0.2) +
	 theme(legend.position = "none") +
       facet_wrap( ~x, scales = "fixed") +
	 xlab("Places") +
	 ylab("Suitability")
	  
###-----------------------------------------------------------------------------------------###

# https://github.com/zonination/perceptions






