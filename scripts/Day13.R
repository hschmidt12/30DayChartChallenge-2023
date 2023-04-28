# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 13 - pop culture

# insider entertainment
# Who is Daddy Roy's favorite this week?

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

library(tidyverse)
library(ggbump)
library(showtext)
library(ggimage)

# load fonts for title
font_add_google("Zen Kaku Gothic New", "zen")
# automatically use showtext when needed
showtext_auto()

# connor, kendall, roman, shiv
colors <- c("#bcaa99","#8e5572","#f2f7f2","#bbbe64")
background <- "#0d1f2d"

df <- read.csv('./data/succession-favorites.csv')
# pivot longer
df <- pivot_longer(df,
                   cols = Kendall:Connor,
                   names_to = "name",
                   values_to = "rank"
)

df$image[df$name == "Kendall"] <- "/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/data/kendall.png"
df$image[df$name == "Shiv"] <- "/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/data/shiv.png"
df$image[df$name == "Roman"] <- "/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/data/roman.png"
df$image[df$name == "Connor"] <- "/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/data/connor.png"

# add label position column
df$labelPos[c(df$name == "Kendall" & df$Week == 9)] <- "here"
df$labelPos[c(df$name == "Connor" & df$Week == 11)] <- "here"
df$labelPos[c(df$name == "Roman" & df$Week == 12)] <- "here"
df$labelPos[c(df$name == "Shiv" & df$Week == 13)] <- "here"

# plot!
ggplot(data = df, aes(x = Week, y = rank, group = name)) +
  geom_bump(smooth = 8, size = 1.25, aes(color = name)) +
  geom_point(data = df %>% filter(Week == min(Week)),
             size = 11,
             aes(color = name)) +
  geom_point(data = df %>% filter(Week == max(Week)),
             size = 11,
             aes(color = name)) +
  geom_image(data = df %>% filter(Week == min(Week)),
             aes(y = rank, x = Week, image = image),
             size = 0.026,
             asp = 2.7) +
  geom_image(data = df %>% filter(Week == max(Week)),
             aes(y = rank, x = Week, image = image),
             size = 0.026,
             asp = 2.7) +
  geom_label(data = df %>% filter(labelPos == "here"),
             aes(label = name, fill = name), color = background) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_y_reverse() +
  labs(x = "\nEPISODE NUMBER", y = "RANK\n",
       title = "WHO IS DADDY'S FAVORITE?",
       subtitle = "Ranking Logan Roy's Favorite Child in Successsion") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20),
                     limits = c(0,33)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        plot.background = element_rect(color = background, fill = background),
        plot.title = element_text(
          family = "zen", 
          size = 20,
          face = "bold", 
          color = 'white'),
        plot.subtitle = element_text(
          family = "zen", 
          size = 14,
          face = "italic",
          color = 'white'),
        axis.text = element_text(
          color = 'white',
          family = 'zen',
          size = 10),
        axis.title = element_text(
          color = 'white',
          family = 'zen',
          face = 'bold'))

# save
ggsave(filename = "./charts/day13_pop-culture.jpeg",
       width = 12,
       height = 5,
       device = "jpeg")

