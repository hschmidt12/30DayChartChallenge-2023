# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 4 - Historical

# Spotify music genre popularity over time (2017 - 2021)

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

# load packages
library(tidyverse)
library(fmsb)
library(magick)

df <- readr::read_csv('https://raw.githubusercontent.com/jaehee99/Music-Genre-Interactive-Visualization/main/data/Spotify_Music.csv')

popularity <- df |>
  group_by(genre, year) |>
  summarize(num.tracks = length(track_name))

# add popularity rank based on number of tracks
popularity <- popularity |>
  group_by(year) |>
  mutate(popularity = dense_rank(desc(num.tracks)))
# remove track numbers
popularity <- popularity[,-3]

# loop through years
years <- c(1990, 1991, 1993, 1994, 1995, 1996, 1998, 1999, 
           2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009,
           2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)

for (i in 1:length(years)) {
  # subset to just one year
  current.year = years[i]
  year <- subset(popularity, year == current.year)
  # pivot wider
  year.wide <- pivot_wider(
    year,
    names_from = genre,
    values_from = popularity
  )
  year.wide <- year.wide[,-1]
  rownames(year.wide) <- "year"
  year.wide <- rbind(rep(6,6) , rep(0,6) , year.wide)
  # define color depending on decade
  ifelse(current.year <= 1999, current.color <- '#D1D646',  
         ifelse(current.year > 1999 & current.year <= 2009, current.color <- '#F97068', current.color <- '#57C4E5'))
  ifelse(current.year <= 1999, current.fill <- rgb(209/255, 214/255, 70/255, 0.4), #rgb to include alpha
         ifelse(current.year > 1999 & current.year <= 2009, current.fill <- rgb(249/255, 112/255, 104/255, 0.4), 
                current.fill <- rgb(87/255, 196/255, 229/255, 0.4)))
  # define decades label depending on year
  ifelse(current.year <= 1999, current.decade <- "1990s",
         ifelse(current.year > 1999 & current.year <= 2009, current.decade <- "2000s", current.decade <- "2010s"))
  # plot
  save.name <- paste('/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/charts/day4/', current.year, '.png', sep = "")
  png(filename = save.name, width = 4, height = 4, units = "in", res = 400)
  radarchart(year.wide,
             #custom polygon
             pcol=current.color, plwd=2, pfcol = current.fill,
             #custom the grid
             cglcol="grey",
             title = paste("popularity of music genres\nreleased in the ",current.decade, " on spotify",sep = ""))
  dev.off()
}

# save as gif
# list file names and read in
dir_out <- "/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/charts/day4"
imgs <- list.files(dir_out, full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023/charts/Day4.gif")

