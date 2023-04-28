# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 2 - Waffle

# US adolescent breakfast consumption 2015 - 2018
# most consumed foods at breakfast on a given day among children and adolescents aged 2 - 19 years old
# data - cdc national center for health statistics

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

# load packages
library(tidyverse)
library(showtext)
library(waffle)
library(patchwork)

# load fonts for title
font_add_google("Rowdies", "rowdies")
# automatically use showtext when needed
showtext_auto()
theme(text=element_text(family="rowdies"))

# define color palette
# milk, cereal, water, pancakes, eggs, juice
colors <- c("#fb5449","#ffa842", "#9cdfe2", "#ffd058", "#54bfd9", "#1e6799")

# manually (ugh) add data from CDC report
age2.5 <- c(Milk = 59.7, Cereal = 26.8, Water = 19.2,
            Waffles = 15.5,
            Eggs = 14.6)

age6.11 <- c(Milk = 47.3, Cereal = 26.1, Water = 19.5,
             Juice = 15.3, Waffles = 12.4)

age12.19 <- c(Milk = 31.5, Water = 19.5, Cereal = 17.3,
              Juice = 8.9, Eggs = 8.8)

# Waffle charts
p1 <- waffle(age2.5, rows = 7,
       title = "2 - 5 Years Old",
       size = 1,
       legend_pos = "bottom",
       colors = c(colors[1], colors[2], colors[3], colors[4], colors[5])) +
  theme(plot.title = element_text(
    family = "rowdies", 
    size = 12,
    color = 'black',
    hjust = 0.5,
    vjust = 0),
    legend.text = element_text(
      family = "rowdies",
      size = 8,
      color = 'black'
    ))

p2 <- waffle(age6.11, rows = 7,
       title = "6 - 11 Years Old",
       size = 1,
       legend_pos = "bottom",
       colors = c(colors[1], colors[2], colors[3], colors[6], colors[4])) +
  theme(plot.title = element_text(
    family = "rowdies", 
    size = 12,
    color = 'black',
    hjust = 0.5,
    vjust = 0),
    legend.text = element_text(
      family = "rowdies",
      size = 8,
      color = 'black'
    ))

p3 <- waffle(age12.19, rows = 6,
       title = "12 - 19 Years Old",
       size = 1,
       legend_pos = "bottom",
       colors = c(colors[1], colors[3], colors[2], colors[6], colors[5])) +
  theme(plot.title = element_text(
          family = "rowdies", 
          size = 12,
          color = 'black',
          hjust = 0.5,
          vjust = 0),
        legend.text = element_text(
          family = "rowdies",
          size = 8,
          color = 'black'
        ))

patchwork <- (p1 + p2 + p3)
patchwork +
  plot_annotation(title = "USA Child & Adolescent Breakfast Consumption 2015 - 2018",
                  caption = "data source - CDC National Center for Health Statistics",
                  theme = theme(plot.caption = element_text(size = 6,
                                                            hjust = 0.5),
                                plot.title = element_text(size = 16,
                                                          hjust = 0.5))) & 
  theme(text = element_text('rowdies')) 

# save
ggsave(filename = "./charts/day2_waffle.jpeg",
       width = 10,
       height = 5,
       device = "jpeg")
