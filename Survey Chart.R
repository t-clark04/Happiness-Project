# Loading in packages that I will need.
library(survey)
library(tidyverse)
library(haven)
library(labelled)

# Loading in my survey data from the General Social Survey.
GSS2022 <- read_dta("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data/GSS2022.dta")

# Now, we need to wrangle the data.
GSS2022_mod <- GSS2022 %>%
  select(finrela, happy, wtssnrps) %>%
  filter(!is.na(finrela) & !is.na(happy))

GSS2022_mod$finrela <- factor(GSS2022_mod$finrela,
                              levels = c(1,2,3,4,5),
                              labels = c("Far Below Average", "Below Average", "Average",
                                         "Above Average", "Far Above Average"))
GSS2022_mod$happy <- factor(GSS2022_mod$happy,
                            levels = c(1,2,3),
                            labels = c("Very Happy", "Pretty Happy", "Not Too Happy"))

# Next, we apply the weights.
GSS2022_w <- svydesign(ids = ~1, data = GSS2022_mod, 
                      weights = GSS2022_mod$wtssnrps)

row <-  as.data.frame(prop.table(svytable(~happy+finrela, 
                                          GSS2022_w), margin = 1))

# Finally, we plot our weighted frequencies.
ggplot(row, aes(x = happy, y = Freq, fill = finrela)) + 
  geom_col(color = "black", position = position_stack(reverse = TRUE), width = 0.8) + 
  ggtitle("U.S. Subjective Happiness Rating by Perceived Relative Income, 2022",
          '"Taken all together, would you say that you are very happy, pretty happy, or not too happy?"') + 
  labs(x = "Subjective Happiness Rating", y = "Weighted Frequency", 
       caption = "Source: General Social Survey, 2022", fill = "Income Perception Compared\nto Avg. U.S. Family ") + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 10.5),
        legend.text = element_text(size = 10.5),
        legend.title = element_text(size = 10.5),
        text = element_text(family = "serif")) + 
  scale_fill_viridis_d()

# Let's try again but flip the independent/dependent variables
GSS2022_w <- svydesign(ids = ~1, data = GSS2022_mod, 
                       weights = GSS2022_mod$wtssnrps)

column <-  as.data.frame(prop.table(svytable(~happy+finrela, 
                                          GSS2022_w), margin = 2))

ggplot(column, aes(x = finrela, y = Freq, fill = happy)) + 
  geom_col(color = "black", width = 0.8) + 
  ggtitle("U.S. Perceived Relative Income vs. Subjective Happiness, 2022",
          '"Compared with American families in general, how would you rank your family income?"') + 
  labs(x = "Perceived Relative Income", y = "Weighted Frequency", 
       caption = "Source: General Social Survey, 2022", fill = "Subjective Happiness Rating") +  
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
  scale_x_discrete(labels = c("Far Below\nAverage", "Below\nAverage", "Average",
                              "Above\nAverage", "Far Above\nAverage")) + 
  scale_fill_viridis_d()

