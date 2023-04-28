# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 10 - hybrid

# HEV Sales by Model (In Order of Market Introduction)
# US Department of Energy : alternative fuels data center
# https://afdc.energy.gov/data/10301

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

# load packages
library(tidyverse)
library(showtext)
library(MetBrewer)
library(gganimate)

# load fonts for title
font_add_google("Arvo", "arvo")
# automatically use showtext when needed
showtext_auto()

palette <- met.brewer("Hiroshige", n = 5)

df <- read.csv("./data/hybrid-data.csv")
df <- pivot_longer(df,
                   cols = X1999:X2019,
                   names_to = "Year",
                   values_to = "Sales")
df$Year <- substring(df$Year, 2)
df$Year <- as.numeric(df$Year)
df$Sales <- as.numeric(df$Sales/1000)

# according to statistica, top 5 car companies in US are:
# 1. Ford 2. Toyota 3. Chevrolet 4. Honda 5. Hyundai
top5 <- c("Ford","Toyota","Chevrolet","Honda","Hyundai")

df <- subset(df, Make %in% top5)

df <- df %>%
  group_by(Year, Make) %>%
  summarize(AvgSales = mean(Sales))

p <- ggplot(df, aes(x = Year, y = AvgSales, group = Make)) +
  geom_line(aes(color = Make), size = 2, lineend = "round") +
  theme_classic() +
  scale_color_manual(values = palette) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7),
                     limits = c(1999,2020)) +
  labs(title = "\nHybrid Electric Vehicle Sales in the U.S. 1999 - 2019\n") +
  xlab(NULL) +
  ylab("Sales (1000s)") +
  guides(fill=guide_legend(ncol=7)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        panel.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        legend.background = element_rect(fill = "#fbf8f4", color = "#fbf8f4"),
        legend.key.height = unit(1, "cm"), 
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = 12),
        text = element_text(size = 15),
        plot.title = element_text(
          size = 20,
          hjust = 1.5,
          color = 'black'
        ))

# save
ggsave(filename = "./charts/day10_hybrid.jpeg",
       plot = p,
       width = 7,
       height = 5,
       device = "jpeg")

# animate!
plot <- p + transition_reveal(df$Year, keep_last = T)

animate(plot, duration = 10, fps = 20, 
        renderer = gifski_renderer(), end_pause = 60,
        height = 5, width = 7, units = "in", res = 150)

# save animation
anim_save("./charts/day10_hybrid.gif")
