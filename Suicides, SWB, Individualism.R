library(dplyr)
library(tidyr)
library(tidyverse)

library(readxl)
culture_dimensions <- read_excel("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data/6-dimensions-for-website-2015-08-16.xls")

individual <- culture_dimensions %>%
  select("ctr", "idv")

suicides_real <- read_excel("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data/suicides_real.xlsx")

suicides <- suicides_real %>%
  pivot_longer(-c("Country Name", "Country Code"),
               names_to = "year",
               values_to = "suicide rate")

suicides <- suicides %>%
  filter(year == 2015)

df <- merge(individual, suicides, by.x = "ctr", by.y = "Country Code")

real_df <- df %>%
  select("ctr", "idv", "suicide rate") %>%
  drop_na()

ggplot(real_df, aes(x = idv, y = `suicide rate`)) +
  geom_point(color = "darkgreen", size = 2) + 
  geom_smooth(method = lm, se = FALSE, linewidth = 1.5) + 
  ggtitle("Country Individualism Measure vs. Suicide Mortality Rate (per 100,000), 2015") + 
  labs(x = "Individualism Measure", y = "Suicide Rate", caption = "Source: World Bank; Geert Hofstede") + 
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
library(haven)

cantril_ladder <- read_csv("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data/happiness-cantril-ladder.csv")


cantril_ladder <- cantril_ladder %>%
  select("Code", "Year", "Cantril ladder score") %>%
  filter(Year == 2015)

second_df <- merge(suicides, cantril_ladder, by.x = "Country Code", by.y = "Code")   %>%
  select("Country Code", "suicide rate", "Cantril ladder score")

ggplot(second_df, aes(x = `suicide rate`, y = `Cantril ladder score`)) +
  geom_point(color = "purple", size = 2) + 
  ggtitle("Suicide Mortality Rate (per 100,000) vs. Average Cantril Ladder Score, 2015") + 
  labs(x = "Suicide Rate", y = "Happiness Rating", caption = "Source: World Bank; Our World in Data") + 
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


third_df <- merge(individual, cantril_ladder, by.x = "ctr", by.y = "Code") %>%
  drop_na()


ggplot(third_df, aes(x = idv, y = `Cantril ladder score`)) +
  geom_point(color = "darkred", size = 2) + 
  ggtitle("Country Individualism Measure vs. Average Cantril Ladder Score, 2015") + 
  labs(x = "Individualism Measure", y = "Happiness Rating", caption = "Source: Geert Hofstede; Our World in Data") + 
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

# Suicides and Marriage Rate by State
setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")

library(readxl)
Marriage <- read_excel("Marriage Rate by State.xlsx")

married <- Marriage %>%
  filter(STATE != "District of Columbia") %>%
  select(STATE, RATE)

colnames(married) <- c("State", "Marriage_Rate")
  
Suicides_by_state <- read_csv("suicides_by_state.csv")

suicides <- Suicides_by_state %>%
  filter(Year == 2019) %>%
  select(State, Rate)

colnames(suicides) <- c("State", "Suicide_Rate")

df <- merge(married, suicides, by = "State")

ggplot(df, aes(x = Marriage_Rate, y = Suicide_Rate)) + 
  geom_point(color="blue") + 
  ggtitle("Suicide Rate vs. Marriage Rate by State, 2019") + 
  labs(x = "Marriage Rate per 1,000 Total Population", y = "Suicide Deaths per 100,000", caption = "Sources: CDC") + 
  theme(plot.title = element_text(hjust = 0.5, color = "black", 
                                  family = "serif", size = 14),
        axis.title.x = element_text(color = "black", family = "serif",
                                    size = 13),
        axis.title.y = element_text(color = "black", family = "serif",
                                    size = 13)) + 
  geom_smooth(method = lm, se = FALSE)
