# My goal here is to determine whether there appears to be a correlation on a
# national level between GDP per capita, human rights, and life expectancy. If,
# as I suspect, there does appear to be a positive correlation between these 
# three variables, then answering the question of what makes people happy across
# nations will be much more difficult to solve, due to multicollinearity among
# predictor variables.

# Loading in packages that I will need.
library(tidyverse)
library(dplyr)
library(haven)
library(readxl)
library(rgl)

# Setting working directory
setwd("C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data")

# Loading in V-Dem data on individual liberties/equality by country in 2021
vdem <- read_dta("V-Dem-CY-Full+Others-v14.dta")
vdem_mod <- vdem %>%
  select(country_name, year, v2xcl_rol) %>%
  filter(year == "2021")

# Loading in and wrangling data from the World Bank on GDP per capita in 2021
gdp <- read_excel("API_NY.GDP.PCAP.CD_DS2_en_excel_v2_55.xlsx")

gdp_new = gdp[,!(names(gdp) %in% c("Country Code", "Indicator Name", "Indicator Code"))]

gdp_mod <- gdp_new %>%
  pivot_longer(-c("Country Name"),
               names_to = "year",
               values_to = "gdp_per_capita")

gdp_mod2 <- gdp_mod %>%
  filter(year == 2021 & !is.na(gdp_per_capita))

# Loading in and wrangling data on life expectancy in 2021
life_exp <- read_excel("API_SP.DYN.LE00.IN_DS2_en_excel_v2_1380.xlsx")

life_exp_new = life_exp[,!(names(life_exp) %in% c("Country Code", "Indicator Name", "Indicator Code"))]

life_exp_mod <- life_exp_new %>%
  pivot_longer(-c("Country Name"),
               names_to = "year",
               values_to = "life_expectancy")

life_exp_mod2 <- life_exp_mod %>%
  filter(year == 2021 & !is.na(life_expectancy))

# Now, we can drop the respective year columns since we know that the year (2021)
# is consistent across data frames
vdem_final <- vdem_mod %>%
  select(country_name, v2xcl_rol)

colnames(vdem_final) <- c("country_name", "equality")

gdp_final <- gdp_mod2 %>%
  select(`Country Name`, gdp_per_capita)

colnames(gdp_final) <- c("country_name", "gdp_per_capita")

life_exp_final <- life_exp_mod2 %>%
  select(`Country Name`, life_expectancy)

colnames(life_exp_final) <- c("country_name", "life_expectancy")

# Let's merge these three data frames into one using the merge function
step_one <- merge(vdem_final, gdp_final, by = "country_name")

df <- merge(step_one, life_exp_final, by = "country_name")

# Now, let's make a 3d plot to look at the correlations between these variables
library(rgl)
plot3d(log(df$gdp_per_capita), df$life_expectancy, df$equality, type = "s", 
       size = 0.75, lit = FALSE, col = "blue",
       xlab = "Logged GDP per Capita", 
       ylab = "Life Expectancy",
       zlab = "Political Equality/Individual Liberty")

play3d(spin3d(axis = c(0,1,1), rpm = 3), duration = 30)

ThreeDMovie <- movie3d(spin3d(rpm=3, axis = c(0,1,1)), duration=30, fps=10, movie = "ThreeDmovieSWB_Real", dir = "C:/Users/t_cla/OneDrive/Visualizing Politics/Project Data") 
