# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 15 - positive/negative

# https://github.com/jamesmartherus/debates
# sentiment presidential debates from democratic and republican nominees

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

library(tidyverse)
library(sentimentr)
library(showtext)
library(debates)
library(patchwork)

data(debate_transcripts)

df <- subset(debate_transcripts, election_year == 2000 | election_year == 2008 | election_year == 2016)
df <- subset(df, speaker == "Al Gore" | speaker == "George W. Bush" | speaker == "John McCain" | speaker == "Barack Obama" |
                 speaker == "Hillary Clinton" | speaker == "Donald Trump")

dialogue <- df |>
  get_sentences() |>
  sentiment()

#### 2000 presidential debates! ####
year2000 <- subset(dialogue, election_year == 2000)
year2000 <- year2000 |>
  group_by(speaker) |>
  mutate(order = 1:length(type))

# just al gore
algore2000 <- subset(year2000, speaker == "Al Gore")
# average over every 10 sentences
Speaker = "Al Gore"
algore.df <- data.frame()
for (i in 1:(round(nrow(algore2000)/10))) {
  if (i == 1) {start = 1}
  test <- subset(algore2000, order >= start & order <= (start + 9))
  Order <- start
  AvgSent <- mean(test$sentiment)
  test <- data.frame(Speaker, Order, AvgSent)
  algore.df <- rbind(algore.df, test)
  start = start + 10}
algore.df$NewOrder <- 1:nrow(algore.df)
# calculate % time above 0
gore.pos <- algore.df |>
  summarize(GorePos = (length(AvgSent[AvgSent > 0]))/length(AvgSent))

# just george bush
bush2000 <- subset(year2000, speaker == "George W. Bush")
# average over every 10 sentences
Speaker = "George W. Bush"
bush.df <- data.frame()
for (i in 1:(round(nrow(bush2000)/10))) {
  if (i == 1) {start = 1}
  test <- subset(bush2000, order >= start & order <= (start + 9))
  Order <- start
  AvgSent <- mean(test$sentiment)
  test <- data.frame(Speaker, Order, AvgSent)
  bush.df <- rbind(bush.df, test)
  start = start + 10}
bush.df$NewOrder <- 1:nrow(bush.df)
# calculate % time above 0
bush.pos <- bush.df |>
  summarize(BushPos = (length(AvgSent[AvgSent > 0]))/length(AvgSent))

# get df with % time each speaker spoke with positive sentiment
pos <- data.frame(Speaker = c("Al Gore", "George W. Bush"),
                  Pos = c(round(gore.pos[1,1]*100, digits = 1), 
                          round(bush.pos[1,1]*100, digits = 1)),
                  NewOrder = c(algore.df$NewOrder[max(algore.df$NewOrder)], 
                               bush.df$NewOrder[max(bush.df$NewOrder)]),
                  AvgSent = c(algore.df$AvgSent[max(algore.df$NewOrder)],
                              bush.df$AvgSent[max(bush.df$NewOrder)]))

# 2000 df
year2000 <- rbind(algore.df, bush.df)

# plotting colors
background <- 'black'
text <- 'white'
parties <- c("#1961fb","#ff3c31")

# plot
p0 <- ggplot(data = year2000, aes(x = NewOrder, y = AvgSent, color = Speaker)) +
  geom_line(size = 1.25) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 2, color = 'white') +
  labs(x = NULL, y = "Sentiment") +
  # label % positive statements
  annotate(geom = "label",
           label = paste0(pos$Pos[1],"% positive"),
           x = 150, y = -0.15,
           size = 5, fill = parties[1], color = text) +
  annotate(geom = "label",
           label = paste0(pos$Pos[2],"% positive"),
           x = 150, y = -0.23,
           size = 5, fill = parties[2], color = text) +
  # add title
  annotate(geom = "text",
           label = "Al Gore",
           x = 10, y = 0.455,
           size = 10, color = parties[1]) +
  annotate(geom = "text",
           label = "vs.",
           x = 32, y = 0.455,
           size = 10, color = text) +
  annotate(geom = "text",
           label = "George W. Bush",
           x = 69, y = 0.455,
           size = 10, color = parties[2]) +
  scale_color_manual(values = parties) +
  scale_fill_manual(values = parties) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        panel.background = element_rect(color = background, fill = background),
        plot.background = element_rect(color = background, fill = background),
        legend.background = element_rect(color = background, fill = background),
        text = element_text(color = text),
        axis.text = element_text(color = text),
        axis.text.x = element_blank(),
        title = element_text(hjust = 0.5,
                             size = 20))

#### 2008 presidential debates! ####
year2008 <- subset(dialogue, election_year == 2008)
year2008 <- year2008 |>
  group_by(speaker) |>
  mutate(order = 1:length(type))

# just barack obama
obama2008 <- subset(year2008, speaker == "Barack Obama")
# average over every 10 sentences
Speaker = "Barack Obama"
obama.df <- data.frame()
for (i in 1:(round(nrow(obama2008)/10))) {
  if (i == 1) {start = 1}
  test <- subset(obama2008, order >= start & order <= (start + 9))
  Order <- start
  AvgSent <- mean(test$sentiment)
  test <- data.frame(Speaker, Order, AvgSent)
  obama.df <- rbind(obama.df, test)
  start = start + 10}
obama.df$NewOrder <- 1:nrow(obama.df)
# calculate % time above 0
obama.pos <- obama.df |>
  summarize(ObamaPos = (length(AvgSent[AvgSent > 0]))/length(AvgSent))

# just john mccain
mccain2008 <- subset(year2008, speaker == "John McCain")
# average over every 10 sentences
Speaker = "John McCain"
mccain.df <- data.frame()
for (i in 1:(round(nrow(mccain2008)/10))) {
  if (i == 1) {start = 1}
  test <- subset(mccain2008, order >= start & order <= (start + 9))
  Order <- start
  AvgSent <- mean(test$sentiment)
  test <- data.frame(Speaker, Order, AvgSent)
  mccain.df <- rbind(mccain.df, test)
  start = start + 10}
mccain.df$NewOrder <- 1:nrow(mccain.df)
# calculate % time above 0
mccain.pos <- mccain.df |>
  summarize(MccainPos = (length(AvgSent[AvgSent > 0]))/length(AvgSent))

# get df with % time each speaker spoke with positive sentiment
pos <- data.frame(Speaker = c("Barack Obama", "John McCain"),
                  Pos = c(round(obama.pos[1,1]*100, digits = 1), 
                          round(mccain.pos[1,1]*100, digits = 1)),
                  NewOrder = c(obama.df$NewOrder[max(obama.df$NewOrder)], 
                               mccain.df$NewOrder[max(mccain.df$NewOrder)]),
                  AvgSent = c(obama.df$AvgSent[max(obama.df$NewOrder)],
                              mccain.df$AvgSent[max(mccain.df$NewOrder)]))

# 2008 df
year2008 <- rbind(obama.df, mccain.df)

# plot
p8 <- ggplot(data = year2008, aes(x = NewOrder, y = AvgSent, color = Speaker)) +
  geom_line(size = 1.25) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 2, color = 'white') +
  labs(x = NULL, y = "Sentiment") +
  # label % positive statements
  annotate(geom = "label",
           label = paste0(pos$Pos[1],"% positive"),
           x = 150, y = -0.2,
           size = 5, fill = parties[1], color = text) +
  annotate(geom = "label",
           label = paste0(pos$Pos[2],"% positive"),
           x = 150, y = -0.27,
           size = 5, fill = parties[2], color = text) +
  # add title
  annotate(geom = "text",
           label = "Barack Obama",
           x = 27, y = 0.455,
           size = 10, color = parties[1]) +
  annotate(geom = "text",
           label = "vs.",
           x = 65, y = 0.455,
           size = 10, color = text) +
  annotate(geom = "text",
           label = "John McCain",
           x = 98, y = 0.455,
           size = 10, color = parties[2]) +
  scale_color_manual(values = parties) +
  scale_fill_manual(values = parties) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        panel.background = element_rect(color = background, fill = background),
        plot.background = element_rect(color = background, fill = background),
        legend.background = element_rect(color = background, fill = background),
        text = element_text(color = text),
        axis.text = element_text(color = text),
        axis.text.x = element_blank(),
        title = element_text(hjust = 0.5,
                             size = 20))

#### 2016 presidential debates! ####
year2016 <- subset(dialogue, election_year == 2016)
year2016 <- year2016 |>
  group_by(speaker) |>
  mutate(order = 1:length(type))

# just hillary clinton
clinton2016 <- subset(year2016, speaker == "Hillary Clinton")
# average over every 10 sentences
Speaker = "Hillary Clinton"
clinton.df <- data.frame()
for (i in 1:(round(nrow(clinton2016)/10))) {
  if (i == 1) {start = 1}
  test <- subset(clinton2016, order >= start & order <= (start + 9))
  Order <- start
  AvgSent <- mean(test$sentiment)
  test <- data.frame(Speaker, Order, AvgSent)
  clinton.df <- rbind(clinton.df, test)
  start = start + 10}
clinton.df$NewOrder <- 1:nrow(clinton.df)
# calculate % time above 0
clinton.pos <- clinton.df |>
  summarize(ClintonPos = (length(AvgSent[AvgSent > 0]))/length(AvgSent))

# just trump
trump2016 <- subset(year2016, speaker == "Donald Trump")
# average over every 10 sentences
Speaker = "Donald Trump"
trump.df <- data.frame()
for (i in 1:(round(nrow(trump2016)/10))) {
  if (i == 1) {start = 1}
  test <- subset(trump2016, order >= start & order <= (start + 9))
  Order <- start
  AvgSent <- mean(test$sentiment)
  test <- data.frame(Speaker, Order, AvgSent)
  trump.df <- rbind(trump.df, test)
  start = start + 10}
trump.df$NewOrder <- 1:nrow(trump.df)
# calculate % time above 0
trump.pos <- trump.df |>
  summarize(TrumpPos = (length(AvgSent[AvgSent > 0]))/length(AvgSent))

# get df with % time each speaker spoke with positive sentiment
pos <- data.frame(Speaker = c("Hillary Clinton", "Donald Trump"),
                  Pos = c(round(clinton.pos[1,1]*100, digits = 1), 
                          round(trump.pos[1,1]*100, digits = 1)),
                  NewOrder = c(clinton.df$NewOrder[max(clinton.df$NewOrder)], 
                               trump.df$NewOrder[max(trump.df$NewOrder)]),
                  AvgSent = c(clinton.df$AvgSent[max(clinton.df$NewOrder)],
                              trump.df$AvgSent[max(trump.df$NewOrder)]))

# 2008 df
year2016 <- rbind(clinton.df, trump.df)

# plot
p16 <- ggplot(data = year2016, aes(x = NewOrder, y = AvgSent, color = Speaker)) +
  geom_line(size = 1.25) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 2, color = 'white') +
  labs(x = NULL, y = "Sentiment") +
  # label % positive statements
  annotate(geom = "label",
           label = paste0(pos$Pos[1],"% positive"),
           x = 190, y = -0.28,
           size = 5, fill = parties[1], color = text) +
  annotate(geom = "label",
           label = paste0(pos$Pos[2],"% positive"),
           x = 190, y = -0.36,
           size = 5, fill = parties[2], color = text) +
  # add title
  annotate(geom = "text",
           label = "Hillary Clinton",
           x = 35, y = 0.455,
           size = 10, color = parties[1]) +
  annotate(geom = "text",
           label = "vs.",
           x = 90, y = 0.455,
           size = 10, color = text) +
  annotate(geom = "text",
           label = "Donald Trump",
           x = 148, y = 0.455,
           size = 10, color = parties[2]) +
  scale_color_manual(values = parties) +
  scale_fill_manual(values = parties) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        panel.background = element_rect(color = background, fill = background),
        plot.background = element_rect(color = background, fill = background),
        legend.background = element_rect(color = background, fill = background),
        text = element_text(color = text),
        axis.text = element_text(color = text),
        axis.text.x = element_blank(),
        title = element_text(hjust = 0.5,
                             size = 20))

# patchwork plot
patchwork <- p0 / p8 / p16
patchwork +
  plot_annotation(title = "Positivity of Potential U.S. Presidents\nSpeaker Sentiment Across Debate Progression",
                  subtitle = "Speaker Sentiment Across Debate ",
                  theme = theme(plot.background = element_rect(color = background, fill = background),
                                plot.title = element_text(size = 32, hjust = 0.5, color = "white")))

# save
ggsave(filename = "./charts/day15_posneg.jpeg",
       width = 10,
       height = 15,
       device = "jpeg")

