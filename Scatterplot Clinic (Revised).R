setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")

library(haven)
library(tidyverse)
library(dplyr)
library(readxl)
library(ggthemes)
library(ggrepel)

WHR2023 <- read_excel("WHR2023.xls")


WHR2023_plot <- ggplot(WHR2023, aes(x = `Logged GDP per capita`, y = `Ladder score`, 
                                    color = `Social support`, label = `Country name`)) + 
  geom_point(size = 2.5) + theme_classic() + 
  ggtitle("Logged GDP per Capita vs. Average Cantril Ladder Score, 2023") + 
  labs(x = "Logged GDP per Capita", y = "Average Ladder Score",
       color = "Measure of\nSocial Support", 
       caption = "Source: World Happiness Report 2023") +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_text(size = 13.5),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) + 
  geom_smooth(method = lm, se = FALSE) + 
  geom_text_repel(data = subset(WHR2023, `Ladder score` > 7.6 | `Ladder score` < 2),
                                segment.size = 0.2, nudge_x = -0.65, size = 3.5,
                                color = "black") + 
  scale_color_viridis_c()
                    
WHR2023_plot






OWID <- read_csv("gdp-vs-happiness.csv")
OWID_2017 <- OWID %>%
  filter(Year == "2017")

OWID_2017_renamed <- rename(OWID_2017, ladder_score = `Cantril ladder score`) 
OWID_2017_renamed2 <- rename(OWID_2017_renamed, GDP_per_capita = `GDP per capita, PPP (constant 2017 international $)`)

OWID2017_plot <- ggplot(OWID_2017_renamed2, aes(x = GDP_per_capita, y = ladder_score)) + 
  geom_point(color = "#7132a8", size = 2)

OWID2017_plot + 
  theme_classic() +
  ggtitle("GDP per Capita vs. Average Happiness Ladder Score, 2017") + 
  labs(x = "GDP per Capita", y = "Average Ladder Score",
       caption = "Source: Our World in Data") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_text(size = 13.5),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) + 
  geom_smooth(method = lm, formula = y ~ log(x), se = FALSE, linewidth = 1.5)
