# Health!!!
GSS2022 <- read_dta("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data/GSS2022.dta")

GSS2022_health <- GSS2022 %>%
  select(health, happy)%>%
  filter(health >= 1 & health <= 4)%>%
  filter(happy >= 1 & happy <= 3) %>%
  filter(!is.na(health) & !is.na(happy))

GSS2022_health$health <- factor(GSS2022_health$health,
                             levels = c(1,2,3,4),
                             labels = c("Excellent", "Good",
                                        "Fair", "Poor"))

GSS2022_health$happy <- (GSS2022_health$happy - 1)

GSS2022_health <- GSS2022_health %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2022_health$true_happy <- as.numeric(GSS2022_health$true_happy)

GSS2022_health_avg <- GSS2022_health %>%
  group_by(health) %>%
  summarize(mean_happiness = mean(happy))

ggplot(GSS2022_health_avg, aes(x = health, y = mean_happiness)) + 
  geom_col(fill = "lightcoral") + 
  scale_y_continuous(limits = c(0, 2)) + 
  labs(y = "Average Subjective Well-Being", x = "Condition of Health", caption = "Source: General Social Survey, 2022") + 
  ggtitle("Health Status vs. Subjective-Well Being", '"Taken all together, would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
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
