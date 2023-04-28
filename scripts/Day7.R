# 30 Day Chart Challenge - 2023
# Helen Schmidt
# Day 7 - hazard

# FEMA national risk index by across US states
# https://hazards.fema.gov/nri/data-resources

# set working directory
setwd("/Volumes/GoogleDrive/My Drive/Schmidt Misc./30DayChartChallenge-2023")

# load packages
library(tidyverse)
library(showtext)
library(ggridges)
library(patchwork)

# load fonts for title
font_add_google("Rowdies", "rowdies")
# automatically use showtext when needed
showtext_auto()

# get overall risk score, avalanche risk score, drought risk, earthquake risk,
# heat wave risk, hurricane risk, river flood risk, tornado risk, wildfire risk
df <- read.csv("./data/risk.csv") |>
  select(STATE, COUNTY, POPULATION, RISK_SCORE, AVLN_RISKS, DRGT_RISKS, ERQK_RISKS, 
         HWAV_RISKS, HRCN_RISKS, RFLD_RISKS, TRND_RISKS, WFIR_RISKS)

# remove territories
df <- subset(df, STATE != "American Samoa" & STATE != "Guam" &
               STATE != "Northern Mariana Islands" & STATE != "Puerto Rico"
             & STATE != "Virgin Islands" & STATE != "District of Columbia")

# add group based on region
northeast <- c("Maine","Vermont","New Hampshire","Massachusetts","Connecticut",
               "Rhode Island","New York","New Jersey","Delaware","Maryland","Pennsylvania")
southeast <- c("Virginia","West Virginia","North Carolina","South Carolina","Georgia",
               "Florida","Alabama","Mississippi","Louisiana","Arkansas","Tennessee","Kentucky")
southwest <- c("Texas","Oklahoma","New Mexico","Arizona")
midwest <- c("Ohio","Indiana","Illinois","Michigan","Wisconsin","Missouri",
             "Iowa","Minnesota","Kansas","Nebraska","South Dakota","North Dakota")
west <- c("Alaska","Hawaii","California","Oregon","Washington","Nevada","Idaho",
          "Montana","Wyoming","Utah","Colorado")

df$REGION[df$STATE %in% northeast] <- "Northeast"
df$REGION[df$STATE %in% southeast] <- "Southeast"
df$REGION[df$STATE %in% southwest] <- "Southwest"
df$REGION[df$STATE %in% midwest] <- "Midwest"
df$REGION[df$STATE %in% west] <- "West"

# colors
colors <- c("#84f404","#00fee2","#ff0087","#dcf900","#ff6400")

# overall region risk
p1 <- ggplot(df, aes(x = RISK_SCORE, y = REGION,
               fill = REGION)) +
  xlab("OVERALL RISK SCORE") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# avalanche risk
av.colors <- c(colors[2],colors[4],colors[5])
p2 <- ggplot(df, aes(x = AVLN_RISKS, y = REGION,
               fill = REGION)) +
  xlab("AVALANCHE RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = av.colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))


# drought risk
p3 <- ggplot(df, aes(x = DRGT_RISKS, y = REGION,
               fill = REGION)) +
  xlab("DROUGHT RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# earthquake risk 
p4 <- ggplot(df, aes(x = ERQK_RISKS, y = REGION,
               fill = REGION)) +
  xlab("EARTHQUAKE RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# heat wave risk 
p5 <- ggplot(df, aes(x = HWAV_RISKS, y = REGION,
               fill = REGION)) +
  xlab("HEAT WAVE RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# hurricane risk 
p6 <- ggplot(df, aes(x = HRCN_RISKS, y = REGION,
               fill = REGION)) +
  xlab("HURRICANE RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# river flood risk 
p7 <- ggplot(df, aes(x = RFLD_RISKS, y = REGION,
               fill = REGION)) +
  xlab("RIVER FLOOD RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# tornado risk 
p8 <- ggplot(df, aes(x = TRND_RISKS, y = REGION,
               fill = REGION)) +
  xlab("TORNADO RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# wildfire risk 
p9 <- ggplot(df, aes(x = WFIR_RISKS, y = REGION,
               fill = REGION)) +
  xlab("WILDFIRE RISK") +
  geom_density_ridges(scale = 1.5, color = 'white') +
  theme_ridges() +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none",
        plot.background = element_rect(color ='#161f2b', fill = '#161f2b'),
        panel.background = element_rect(color = '#161f2b', fill = '#161f2b'),
        axis.text = element_text(color = 'white'),
        axis.title = element_text(color = 'white'))

# patchwork plot
patchwork <- (p1 + p2 + p3) / (p4 + p5 + p6) / (p7 + p8 + p9)
patchwork +
  plot_annotation(title = "\nNatural Hazard Risks In Regions of the United States\n",
                  caption = "data source - FEMA National Risk Index",
                  theme = theme(plot.caption = element_text(size = 10, hjust = 0.5, color = "white", face = "bold"),
                                plot.background = element_rect(color = '#161f2b', fill = '#161f2b'),
                                plot.title = element_text(size = 16, hjust = 0.5, color = "white", face = "bold")))

# save
ggsave(filename = "./charts/day7_hazard.jpeg",
       width = 10,
       height = 8,
       device = "jpeg")

