# Work satisfaction !!!!!!!!
setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")

library(haven)
GSS2022 <- read_dta("GSS2022.dta")
library(tidyverse)
library(dplyr)

GSS2022_work <- GSS2022 %>%
  select(satjob, happy) %>%
  filter(satjob >= 1 & satjob <= 4) %>%
  filter(happy >=1 & happy <=3) %>%
  drop_na()

GSS2022_work$satjob <- factor(GSS2022_work$satjob,
                              levels = c(1,2,3,4),
                              labels = c("Very satisfied", "Moderately satisfied",
                                         "A little dissatisfied", "Very dissatisfied"))

GSS2022_work$happy <- (GSS2022_work$happy - 1)

GSS2022_work <- GSS2022_work %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2022_work$true_happy <- as.numeric(GSS2022_work$true_happy)

GSS2022_work_avg <- GSS2022_work %>%
  group_by(satjob) %>%
  summarize(mean_happiness = mean(true_happy))

library(forcats)

ggplot(GSS2022_work_avg, aes(x = fct_rev(satjob), y = mean_happiness)) + 
  geom_col(fill = "darkred") + 
  scale_y_continuous(limits = c(0, 2)) + 
  labs(y = "Average Subjective Well-Being", x = "Job Satisfaction", caption = "Source: General Social Survey, 2022") + 
  ggtitle("Job Satisfaction vs. Subjective-Well Being", '"Taken all together, would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_text(size = 13.5),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) + coord_flip()
