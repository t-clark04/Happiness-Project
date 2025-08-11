setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")
library(haven)
library(dplyr)
library(tidyverse)

# Loading and Wrangling the 1980 GSS
GSS1980 <- read_dta("GSS1980.dta")

GSS1980_mod <- GSS1980 %>%
  select(year, happy, marital) %>%
  filter(marital == 1 | marital == 2 | marital == 3 | marital == 4 | 
           marital == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS1980_mod$marital <- factor(GSS1980_mod$marital)

# Loading and Wrangling the 1990 GSS
GSS1990 <- read_dta("GSS1990.dta")

GSS1990_mod <- GSS1990 %>%
  select(year, happy, marital) %>%
  filter(marital == 1 | marital == 2 | marital == 3 | marital == 4 | 
           marital == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS1990_mod$marital <- factor(GSS1990_mod$marital)

# Loading and Wrangling the 2000 GSS
GSS2000 <- read_dta("GSS2000.dta")

GSS2000_mod <- GSS2000 %>%
  select(year, happy, marital) %>%
  filter(marital == 1 | marital == 2 | marital == 3 | marital == 4 | 
           marital == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2000_mod$marital <- factor(GSS2000_mod$marital)

#Loading and Wrangling the 2010 GSS
GSS2010 <- read_dta("GSS2010.dta")

GSS2010_mod <- GSS2010 %>%
  select(year, happy, marital) %>%
  filter(marital == 1 | marital == 2 | marital == 3 | marital == 4 | 
           marital == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2010_mod$marital <- factor(GSS2010_mod$marital)

#Loading and Wrangling the 2022 GSS
GSS2022 <- read_dta("GSS2022.dta")

GSS2022_mod <- GSS2022 %>%
  select(year, happy, marital) %>%
  filter(marital == 1 | marital == 2 | marital == 3 | marital == 4 | 
           marital == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2022_mod$marital <- factor(GSS2022_mod$marital)

GSScomb_mod <- rbind(GSS1980_mod, GSS1990_mod, GSS2000_mod, GSS2010_mod, GSS2022_mod)

GSScomb_mod$marital <- factor(GSScomb_mod$marital,
                              levels = c(1,2,3,4,5),
                              labels = c("Married", "Widowed", "Divorced", 
                                          "Separated", "Never Married"))

GSScomb_mod$happy <- (GSScomb_mod$happy - 1)

GSScomb_mod <- GSScomb_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSScomb_mod$true_happy <- as.numeric(GSScomb_mod$true_happy)


# Making a new dataframe with the averages I want
GSS_means <- GSScomb_mod %>%
  group_by(year, marital) %>%
  summarize(mean_happiness = mean(true_happy))

# Making a Line Graph With the Data
ggplot(GSS_means, aes(x = year, y = mean_happiness, color = marital)) + 
  geom_line(linewidth = 1.25) + 
  geom_point() + 
  scale_y_continuous(limits = c(0,2)) + 
  ggtitle("Average U.S. Happiness Rating Over Time, 1980-2022", 
          '"Would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
  labs(x = "Year", y = "Average Happiness Rating", color = "Marital Status",
       caption = "Source: General Social Survey") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_text(size = 13.5),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"))
