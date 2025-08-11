setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")

library(haven)
GSS2022 <- read_dta("GSS2022.dta")

library(tidyverse)
library(dplyr)

# AGE!!!!!!!!!!!

GSS2022_age <- GSS2022 %>%
  select(age, happy) %>%
  filter(happy == 1 | happy == 2 | happy == 3) %>%
  filter(age >= 18) %>%
  drop_na() %>%
  mutate(
    age_group = case_when(
      age <= 19 ~ "0-19",
      age >= 20 & age <= 29 ~ "20-29",
      age >= 30 & age <= 39 ~ "30-39",
      age >= 40 & age <= 49 ~ "40-49",
      age >= 50 & age <= 59 ~ "50-59",
      age >= 60 & age <= 69 ~ "60-69",
      age >= 70 & age <= 79 ~ "70-79",
      age >= 80  ~ "80+"
    ))

GSS2022_age$happy <- (GSS2022_age$happy - 1)

GSS2022_age_mod <- GSS2022_age %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2022_age_mod$true_happy <- as.numeric(GSS2022_age_mod$true_happy)
    


GSS2022_age_avg <- GSS2022_age_mod %>%
  group_by(age_group) %>%
  summarize(mean_happiness = mean(true_happy))

GSS2022_age_avg$age_group <- factor(GSS2022_age_avg$age_group,
                                    levels = c("0-19", "20-29", "30-39",  "40-49", 
                                               "50-59", "60-69", "70-79", "80+"))

age_plot <- ggplot(GSS2022_age_avg, aes(x = age_group, y = mean_happiness)) + 
  geom_col(fill = "cadetblue") + 
  scale_y_continuous(limits = c(0,2)) + 
  labs(y = "Average Subjective Well-Being", x = "Age Group", caption = "Source: General Social Survey, 2022") + 
  ggtitle("Age vs. Subjective Well-Being", '"Taken all together, would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
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

age_plot

# SEX!!!!!!!!
GSS_sex <- GSS2022 %>%
  select(sex, happy) %>%
  drop_na()

GSS_sex$happy <- (GSS_sex$happy - 1)

GSS_sex_mod <- GSS_sex %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS_sex_mod$true_happy <- as.numeric(GSS_sex_mod$true_happy)

GSS_sex_avg <- GSS_sex_mod %>%
  group_by(sex) %>%
  summarize(mean_happiness = mean(true_happy))

GSS_sex_avg$sex <- factor(GSS_sex_avg$sex,
                          levels = c("1", "2"),
                          labels = c("Male", "Female"))

sex_plot <-ggplot(GSS_sex_avg, aes(x = sex, y = mean_happiness)) + 
  geom_col(fill = "darkslateblue") + 
  scale_y_continuous(limits = c(0, 2)) + 
  labs(y = "Average Subjective Well-Being", x = "Sex", caption = "Source: General Social Survey, 2022") + 
  ggtitle("Sex vs. Subjective Well-Being", '"Taken all together, would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
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

sex_plot

# RACE !!!!!!!!!!!!!!!!!!
GSS_race <- GSS2022 %>%
  select(race, happy) %>%
  filter(happy == 1 | happy == 2 | happy == 3) %>%
  filter(race == 1 | race == 2 | race == 3) %>%
  drop_na()

GSS_race$happy <- (GSS_race$happy - 1)

GSS_race_mod <- GSS_race %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS_race_mod$true_happy <- as.numeric(GSS_race_mod$true_happy)

GSS_race_mod$race <- factor(GSS_race_mod$race,
                          levels = c("1", "2", "3"),
                          labels = c("White", "Black", "Other"))

GSS_race_avg <- GSS_race_mod %>%
  group_by(race) %>%
  summarize(mean_happiness = mean(true_happy))

race_plot <- ggplot(GSS_race_avg, aes(x = race, y = mean_happiness)) + 
  geom_col(fill = "sienna2") + 
  scale_y_continuous(limits = c(0, 2)) + 
  labs(y = "Average Subjective Well-Being", x = "Race", caption = "Source: General Social Survey, 2022") + 
  ggtitle("Race vs. Subjective Well-Being", '"Taken all together, would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
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

race_plot

# Grid Arrange
library(gridExtra)
library(tidyverse)

Combined <- grid.arrange(age_plot, sex_plot, race_plot, ncol = 2)

# Education Level !!!!!!!!!
# 1980 data
GSS1980 <- read_dta("GSS1980.dta")

GSS1980_mod <- GSS1980 %>%
  select(year, happy, degree) %>%
  filter(degree == 0 | degree == 1 | degree == 2 | degree == 3 | 
           degree == 4) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS1980_mod$degree <- factor(GSS1980_mod$degree)

GSS1980_mod$happy <- (GSS1980_mod$happy - 1)

GSS1980_mod <- GSS1980_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS1980_mod$true_happy <- as.numeric(GSS1980_mod$true_happy)

# 1990 data
GSS1990 <- read_dta("GSS1990.dta")

GSS1990_mod <- GSS1990 %>%
  select(year, happy, degree) %>%
  filter(degree == 0 | degree == 1 | degree == 2 | degree == 3 | 
           degree == 4) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS1990_mod$degree <- factor(GSS1990_mod$degree)

GSS1990_mod$happy <- (GSS1990_mod$happy - 1)

GSS1990_mod <- GSS1990_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS1990_mod$true_happy <- as.numeric(GSS1990_mod$true_happy)

# 2000 data
GSS2000 <- read_dta("GSS2000.dta")

GSS2000_mod <- GSS2000 %>%
  select(year, happy, degree) %>%
  filter(degree == 0 | degree == 1 | degree == 2 | degree == 3 | 
           degree == 4) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2000_mod$degree <- factor(GSS2000_mod$degree)

GSS2000_mod$happy <- (GSS2000_mod$happy - 1)

GSS2000_mod <- GSS2000_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2000_mod$true_happy <- as.numeric(GSS2000_mod$true_happy)

# 2010 data
GSS2010 <- read_dta("GSS2010.dta")

GSS2010_mod <- GSS2010 %>%
  select(year, happy, degree) %>%
  filter(degree == 0 | degree == 1 | degree == 2 | degree == 3 | 
           degree == 4) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2010_mod$degree <- factor(GSS2010_mod$degree)

GSS2010_mod$happy <- (GSS2010_mod$happy - 1)

GSS2010_mod <- GSS2010_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2010_mod$true_happy <- as.numeric(GSS2010_mod$true_happy)

# 2022 data
GSS2022 <- read_dta("GSS2022.dta")

GSS2022_mod <- GSS2022 %>%
  select(year, happy, degree) %>%
  filter(degree == 0 | degree == 1 | degree == 2 | degree == 3 | 
           degree == 4) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2022_mod$degree <- factor(GSS2022_mod$degree)

GSS2022_mod$happy <- (GSS2022_mod$happy - 1)

GSS2022_mod <- GSS2022_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2022_mod$true_happy <- as.numeric(GSS2022_mod$true_happy)

# combining the data
GSScomb_mod <- rbind(GSS1980_mod, GSS1990_mod, GSS2000_mod, GSS2010_mod, GSS2022_mod)

GSScomb_mod$degree <- factor(GSScomb_mod$degree,
                              levels = c(0,1,2,3,4),
                              labels = c("Less than high school", "High school", "Associate/junior college", 
                                         "Bachelor's", "Graduate"))

GSS_means <- GSScomb_mod %>%
  group_by(year, degree) %>%
  summarize(mean_happiness = mean(true_happy))

# line graph
ggplot(GSS_means, aes(x = year, y = mean_happiness, color = degree)) + 
  geom_line(linewidth = 1.35) + 
  geom_point() + 
  scale_y_continuous(limits = c(0,2)) + 
  ggtitle("Average U.S. Happiness Rating Over Time, 1980-2022", 
          '"Would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
  labs(x = "Year", y = "Average Happiness Rating", color = "Highest Educational Degree",
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
        axis.text.y = element_text(size = 12, color = "black")) + 
  scale_color_manual(values = c("blue", "darkgreen", "red", "purple", "darkorange"))


# EMPLOYMENT STATUS!!!!!!!!!!!!!!!!!
# 1980 data
GSS1980 <- read_dta("GSS1980.dta")

GSS1980_mod <- GSS1980 %>%
  select(year, happy, wrkstat) %>%
  filter(wrkstat == 1 | wrkstat == 2 | wrkstat == 4 | wrkstat == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS1980_mod$wrkstat <- factor(GSS1980_mod$wrkstat)

# 1990 data
GSS1990 <- read_dta("GSS1990.dta")

GSS1990_mod <- GSS1990 %>%
  select(year, happy, wrkstat) %>%
  filter(wrkstat == 1 | wrkstat == 2 | wrkstat == 4 | wrkstat == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS1990_mod$wrkstat <- factor(GSS1990_mod$wrkstat)

# 2000 data
GSS2000 <- read_dta("GSS2000.dta")

GSS2000_mod <- GSS2000 %>%
  select(year, happy, wrkstat) %>%
  filter(wrkstat == 1 | wrkstat == 2 | wrkstat == 4 | wrkstat == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2000_mod$wrkstat <- factor(GSS2000_mod$wrkstat)

# 2010 data
GSS2010 <- read_dta("GSS2010.dta")

GSS2010_mod <- GSS2010 %>%
  select(year, happy, wrkstat) %>%
  filter(wrkstat == 1 | wrkstat == 2 | wrkstat == 4 | wrkstat == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2010_mod$wrkstat <- factor(GSS2010_mod$wrkstat)

# 2022 data
GSS2022 <- read_dta("GSS2022.dta")

GSS2022_mod <- GSS2022 %>%
  select(year, happy, wrkstat) %>%
  filter(wrkstat == 1 | wrkstat == 2 | wrkstat == 4 | wrkstat == 5) %>%
  filter(happy == 1 | happy == 2 | happy == 3)

GSS2022_mod$wrkstat <- factor(GSS2022_mod$wrkstat)

# combining the data
GSScomb_mod <- rbind(GSS1980_mod, GSS1990_mod, GSS2000_mod, GSS2010_mod, GSS2022_mod)

GSScomb_mod$wrkstat <- factor(GSScomb_mod$wrkstat,
                             levels = c(1,2,4,5),
                             labels = c("Working full time", "Working part time", 
                                        "Unemployed, laid off, looking for work", "Retired")) 

GSScomb_mod$happy <- (GSScomb_mod$happy - 1)

GSScomb_mod <- GSScomb_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSScomb_mod$true_happy <- as.numeric(GSScomb_mod$true_happy)

GSS_means <- GSScomb_mod %>%
  group_by(year, wrkstat) %>%
  summarize(mean_happiness = mean(true_happy))

# line graph
ggplot(GSS_means, aes(x = year, y = mean_happiness, color = wrkstat)) + 
  geom_line(linewidth = 1) + 
  geom_point() + 
  scale_y_continuous(limits = c(0,2)) + 
  ggtitle("Average U.S. Happiness Rating Over Time, 1980-2022", 
          '"Would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
  labs(x = "Year", y = "Average Happiness Rating", color = "Employment Status",
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
        axis.text.y = element_text(size = 12, color = "black")) + 
  scale_color_manual(values = c("blue", "red", "purple", "darkorange"))

# bar chart
GSS_work <- GSS2022 %>%
  select(wrkstat, happy) %>%
  drop_na()

GSS_work$wrkstat <- factor(GSS_work$wrkstat,
                        levels = c("1", "2", "4", "5"),
                        labels = c("Working full time", "Working part time", 
                                   "Unemployed, laid off, looking for work", "Retired"))

GSS_work <- GSS_work %>%
  drop_na()

GSS_work_avg <- GSS_work %>%
  filter(happy == 1 | happy == 2 | happy == 3) %>%
  group_by(wrkstat) %>%
  summarize(mean_happiness = mean(happy))

ggplot(GSS_work_avg, aes(x = wrkstat, y = mean_happiness)) + 
  geom_col(fill = "violetred") + 
  scale_y_continuous(limits = c(0, 3)) + 
  scale_x_discrete(labels = c("Working full time", "Working part time",
                                "Unemployed, laid off,\nlooking for work", "Retired")) +
  labs(y = "Average Subjective Well-Being", x = "Employment Status", caption = "Source: General Social Survey, 2022") + 
  ggtitle("Employment Status vs. Subjective-Well Being", '"Taken all together, would you say that you are very happy (1), pretty happy (2), or not too happy (3)?"') + 
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10, color = "black"),
        plot.caption = element_text(size = 10.5),
        legend.text = element_text(size = 10.5),
        legend.title = element_text(size = 10.5),
        text = element_text(family = "serif")) + 
  coord_flip()
