# Friendship
WVS_2017 <- read_dta("WVS_Wave_7_United_States_Stata_v5.0.dta")

WVS_2017_friend <- WVS_2017 %>%
  select(Q49, Q2) %>%
  filter(Q2 >= 1 & Q2 <= 4) %>%
  filter(Q49 >= 1 & Q49 <= 10) %>%
  drop_na()

WVS_2017_friend$Q2 <- factor(WVS_2017_friend$Q2,
                             levels = c(1:4),
                             labels = c("Very important", "Rather important", "Not very important",
                                        "Not at all important"))


data <- WVS_2017_friend %>%
  group_by(Q2) %>%
  summarize(mean_happiness = mean(Q49))

ggplot(data, aes(x = Q2, y = mean_happiness)) + 
  geom_col(fill = "lightgreen") + 
  scale_y_continuous(limits = c(0, 10)) + 
  labs(y = "Average Life Satisfaction", x = "Important in life: Friends", caption = "Source: World Values Survey, 2017") + 
  ggtitle("Friendship Prioritization vs. Subjective-Well Being", '"All things considered, how satisfied are you with your life as a whole these days (1 = low, 10 = high)?"') + 
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