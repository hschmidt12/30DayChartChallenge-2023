# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 3 - Flora/Fauna

# Anderson's iris data set
# https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

# load packages
library(tidyverse)
library(showtext)
library(dendextend)
library(colorspace)

# load fonts for title
font_add_google("Rowdies", "rowdies")
# automatically use showtext when needed
showtext_auto()
theme(text=element_text(family="rowdies"))

data(iris)
# remove species
iris2 <- iris[,-5]

d.iris <- dist(iris2)
hc.iris <- hclust(d.iris, method = "complete")
iris.species <- rev(levels(iris[,5]))

dend <- as.dendrogram(hc.iris)
dend <- rotate(dend, 1:150)
dend <- color_branches(dend, k=3 , groupLabels=iris.species)
labels_colors(dend) 
labels_colors <-
  rainbow_hcl(3)[sort_levels_values(
    as.numeric(iris[,5])[order.dendrogram(dend)]
  )]

colors <- c("blue", "red", "green")

plot(dend)
ggraph(dend, layout = "dendrogram", circular = T) + 
  geom_edge_diagonal(aes(color = labels_colors)) +
  #scale_edge_colour_distiller(palette = "RdPu") +
  theme_void() 

# Make the plot
ggraph(dend, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(colour="grey") +
  scale_edge_colour_distiller(palette = "RdPu") +
  theme_void() 
