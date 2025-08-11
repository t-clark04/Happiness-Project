# Loading in packages
library(readr)
library(tidyverse)
library(dplyr)
library(plotly)


# Setting working directory
setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")


# Loading in my first data set -- human development index from Our World in Data
human_index <- read_csv("human-development-index.csv")

human_index_mod <- human_index %>%
  filter(Year == 2019) %>%
  select("Code", "Human Development Index")


# Now, let's load in my second data set -- happiness rating also from Our World in Data
cantril_score <- read_csv("happiness-cantril-ladder.csv")

cantril_score_mod <- cantril_score %>%
  filter(Year == 2019) %>%
  select("Code", "Cantril ladder score", "Entity")


# Merging into one dataframe
df <- merge(human_index_mod, cantril_score_mod, by = "Code")


# I also need a dataframe that matches country codes to regions
continents <- read_csv("continents2.csv")

continents <- continents %>%
  select("alpha-3", "region")

colnames(continents) <- c("Code", "Region")

continents$Region <- factor(continents$Region)


# Let's merge these together
finaldf <- merge(df, continents, by = "Code")

colnames(finaldf) <- c("Code", "HDI", "SWB", "Country", "Region")


# Okay, now we can make an animated graph using ggplotly
scatter <- ggplot(finaldf, aes(x = HDI,
                               y = SWB,
                               color = Region,
                               frame = Region,
                               ids = Country)) + 
  geom_point(size = 2) + 
  ggtitle("National Subjective Well-Being vs. Human Development Index Score") + 
  labs(x = "Human Development Index Score", y = "Average Subjective Well-Being",
       color = "", caption = "Source: Our World in Data") + 
  theme_light() + 
  theme(legend.position = "none",
        axis.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))

ggplotly(scatter, width=800, height=500, 
         tooltip = c("Country", "HDI", "SWB")) %>%
  animation_opts(frame = 2000, transition = 0)

