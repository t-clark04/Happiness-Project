library(haven)
library(tidyverse)
library(dplyr)

WVS_2017 <- read_dta("WVS_Wave_7_United_States_Stata_v5.0.dta")

WVS_2017_red <- WVS_2017 %>%
  select(Q49, Q288) %>%
  filter(Q288 >= 1 & Q288 <= 10) %>%
  filter(Q49 >= 1 & Q49 <= 10) %>%
  drop_na()


WVS_2017_red$Q288 <- factor(WVS_2017_red$Q288,
                           levels = c(1:10),
                           labels = c("1-10", "11-20", "21-30", "31-40", "41-50",
                                      "51-60", "61-70", "71-80", "81-90", "91-100"))

dataframe <- WVS_2017_red %>%
  group_by(Q288) %>%
  summarize(mean_happiness = mean(Q49))

ggplot(dataframe, aes(x = Q288, y = mean_happiness)) + 
  geom_col(fill = "darkred") + 
  scale_y_continuous(limits = c(0, 10)) + 
  labs(y = "Average Life Satisfaction", x = "Income Quantile", caption = "Source: World Values Survey, 2017") + 
  ggtitle("Income Group vs. Subjective-Well Being", '"All things considered, how satisfied are you with your life as a whole these days (1 = low, 10 = high)?"') + 
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
