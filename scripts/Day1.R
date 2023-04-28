# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 1 - Part to Whole

# breakdown of colors included in all country flags
# https://archive.ics.uci.edu/ml/datasets/Flags

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

# load packages
library(tidyverse)
library(ggraph)
library(igraph)
library(showtext)

# load fonts for title
font_add_google("Comfortaa", "comfortaa")
# automatically use showtext when needed
showtext_auto()

# read in flag data
df <- read.csv("./data/flag.data")
# select only country names and color columns
df <- df[,c(1,11,12,13,14,15,16,17,18)]
df <- rbind(names(df), df)
# rename columns to match colors
names(df)[1] <- "country"
names(df)[2] <- "red"
names(df)[3] <- "green"
names(df)[4] <- "blue"
names(df)[5] <- "gold"
names(df)[6] <- "white"
names(df)[7] <- "black"
names(df)[8] <- "orange"
names(df)[9] <- "main_hue"
  
# create palette
#colors <- c("#cf1300", "#ffffff", "#41478a", "#f8be00", "#537300", "#000000", "#e96700")
# order - red, white, blue, gold, green, black, orange
colors <- c("#000000","#41478a","#f8be00","#537300", "#e96700","#cf1300", "#ffffff")
  
# fix the first row
AFG <- c("Afghanistan", 1, 1, 0, 1, 1, 1, 0, "green")
df <- rbind(AFG, df)
# remove second redundant row
df <- df[-2,]

# pivot longer
df.long <- pivot_longer(df,
                     cols = red:orange,
                     names_to = "color",
                     values_to = "freq")
# make freq numeric
df.long$freq <- as.numeric(df.long$freq)
# remove instances of zero
df.long <- subset(df.long, freq != 0)

# summarize 
test <- df.long %>%
  group_by(color) %>%
  summarize(frequency = sum(freq))

# sort data frame into descending order
test <- test[order(-test$frequency),]

# Compute percentages
test$fraction = test$frequency / sum(test$frequency)
# Compute the cumulative percentages (top of each rectangle)
test$ymax = cumsum(test$fraction)
# Compute the bottom of each rectangle
test$ymin = c(0, head(test$ymax, n=-1))

# plot!
# frequency of colors in world flags
ggplot(test, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = color)) +
  geom_rect() +
  scale_fill_manual(values = colors) +
  coord_polar(theta = "y") +
  xlim(c(1, 4)) +
  theme_void() +
  labs(title = "Color Palette of World Flags",
       caption = "data source - UCI Machine Learning Repository") +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#ebecf0", color = "#ebecf0"),
        panel.background = element_rect(fill = "#ebecf0", color = "#ebecf0"),
        plot.title = element_text(
          family = "comfortaa", 
          size = 16,
          face = "bold", 
          color = 'black',
          hjust = 0.5,
          vjust = -4),
        plot.caption = element_text(
          family = "comfortaa",
          size = 6,
          color = 'black',
          hjust = 0.5,
          vjust = 15
        ))

# save
ggsave(filename = "./charts/day1_part-to-whole.jpeg",
       width = 5,
       height = 5,
       device = "jpeg")

