# Loading in packages that I will need.
library(survey)
library(tidyverse)
library(haven)
library(labelled)

setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")

# Loading in my survey data from the General Social Survey.
GSS2022 <- read_dta("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data/GSS2022.dta")

# Now, we need to wrangle the data.
GSS2022_mod <- GSS2022 %>%
  select(relig, attend, reliten, happy, wtssnrps)%>%
  mutate(
    is_relig = case_when(
      relig == 1 ~ "Yes",
      relig == 2 ~ "Yes",
      relig == 3 ~ "Yes",
      relig == 4 ~ "No",
      relig == 5 ~ "Yes",
      relig == 6 ~ "Yes",
      relig == 7 ~ "Yes",
      relig == 8 ~ "Yes",
      relig == 9 ~ "Yes",
      relig == 10 ~ "Yes",
      relig == 11 ~ "Yes",
      relig == 12 ~ "Yes",
      relig == 13 ~ "Yes")) %>%
  filter(!is.na(relig) & !is.na(happy))

GSS2022_mod$is_relig <- factor(GSS2022_mod$is_relig)

GSS2022_mod$happy <- factor(GSS2022_mod$happy,
                            levels = c(1,2,3),
                            labels = c("Very Happy", "Pretty Happy", "Not Too Happy"))

# Apply the weights
GSS2022_w <- svydesign(ids = ~1, data = GSS2022_mod, 
                       weights = GSS2022_mod$wtssnrps)

column <-  as.data.frame(prop.table(svytable(~happy+is_relig, 
                                             GSS2022_w), margin = 2))

ggplot(column, aes(x = is_relig, y = Freq, fill = happy)) + 
  geom_col(color = "black", width = 0.8) + 
  ggtitle("Religious Affiliation vs. Subjective Happiness, 2022") + 
  labs(x = "Religiously Affiliated?", y = "Weighted Frequency", 
       caption = "Source: General Social Survey, 2022", fill = "Happiness Rating") +  
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
  scale_fill_brewer(palette = "Set1")

# Strength of Affiliation
GSS2018 <- read_dta("GSS2018.dta")

GSS2018_mod <- GSS2018 %>%
  select(reliten, happy) %>%
  filter(reliten >= 1 & reliten <= 4) %>%
  filter(happy >= 1 & happy <= 3)  %>% 
  drop_na()

GSS2018_mod$reliten <- factor(GSS2018_mod$reliten,
                             levels = c(4,3,2,1),
                             labels = c("No religion", "Somewhat strong", "Not very strong", "Strong"))

GSS2018_mod$happy <- (GSS2018_mod$happy - 1)

GSS2018_mod <- GSS2018_mod %>%
  mutate(
    true_happy = case_when(
      happy == 0 ~ "2",
      happy == 1 ~ "1",
      happy == 2 ~ "0"))

GSS2018_mod$true_happy <- as.numeric(GSS2018_mod$true_happy)

GSS2018_mod_avg <- GSS2018_mod %>%
  group_by(reliten) %>%
  summarize(mean_happiness = mean(true_happy))

ggplot(GSS2018_mod_avg, aes(x = reorder(reliten, mean_happiness), y = mean_happiness)) + 
  geom_col(fill = "lavenderblush3") + 
  scale_y_continuous(limits = c(0, 2)) + 
  labs(y = "Average Subjective Well-Being", x = "Strength of Religious Affiliation", caption = "Source: General Social Survey, 2018") + 
  ggtitle("Religious Affiliation Strength vs. Subjective-Well Being", '"Taken all together, would you say that you are very happy (2), pretty happy (1), or not too happy (0)?"') + 
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
