# Perceived Happiness of College Students
library(readxl)
library(tidyverse)
setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")
college <- read_excel("Perceived Happiness of College Students.xlsx")
View(college)

ggplot(college, aes(x = choice, y = percent, fill = sex)) + 
  geom_col(position = "dodge") + 
  scale_x_discrete(labels = c("Falling in Love", "Fame/Prestige",
                              "Physical Pleasures", "Winning the Lottery")) +
  labs(x = "Survey Item", y = "Relative Frequency", fill = "Sex", 
       caption = "Source: Pettijohn and Pettijohn, 1996") + 
  ggtitle("First-Ranked Choice of Need by 150 College Students") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        axis.title.x = element_text(size = 13, vjust = -3),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 11, color = "black"),
        plot.caption = element_text(size = 11.5),
        legend.text = element_text(size = 11.5),
        legend.title = element_text(size = 11.5),
        text = element_text(family = "serif")) + 
  scale_fill_brewer(palette = "Pastel1")

# Happier with more money?
library(forcats)
money <- read_excel("Happier with More Money.xlsx")
money$Likelihood <- factor(money$Likelihood, 
                           levels = c("More likely", "Somewhat likely", "Less likely", "Unsure"))

ggplot(money, aes(x = Likelihood, y = percent)) + 
  geom_col(fill = "springgreen3") +
  labs(y = "Relative Frequency", caption = "Source: Becoming Minimalist") + 
  ggtitle("How likely are you to be happier in life if you had more money?") + 
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

# Work vs. Retirement
Work_or_Retire <- read_excel("Work or Retire.xlsx")

ggplot(Work_or_Retire, aes(x = fct_rev(reorder(Goal, percent)), y = percent)) + 
  geom_col(fill = "turquoise3") +
  labs(y = "Relative Frequency", caption = "Source: Becoming Minimalist") + 
  ggtitle("Which is a more attractive goal:to retire early and live \na life of leisure or to work a long time at a job you find fulfilling and productive?
") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black")) +
  scale_x_discrete(labels = c("Retire early and live\na life of leisure",
                              "Work a long time at a job\nyou find fulfilling and productive", "Unsure"))
                            

# Lottery question
Lottery_Question <- read_excel("Lottery Question.xlsx")

Lottery_Question$Response <- factor(Lottery_Question$Response,
                                       levels = c("Yes", "No", "Unsure"))

ggplot(Lottery_Question, aes(x = Response, y = percent)) + 
  geom_col(fill = "palevioletred2") +
  labs(y = "Relative Frequency", caption = "Source: Becoming Minimalist") + 
  ggtitle("If you won the lottery today, would you quit your job?") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"))

# Desires or Helping Others
Desires <- read_excel("Desires or Helping Others.xlsx")

ggplot(Desires, aes(x = Response, y = percent)) + 
  geom_col(fill = "purple3") +
  labs(y = "Relative Frequency", caption = "Source: Becoming Minimalist") + 
  ggtitle("Which generally gives you greater joy: fulfilling your own desires or helping other people?") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        text = element_text(family = "serif"),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        plot.caption = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13.5),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"))
