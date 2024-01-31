##### Statistical analysis in R Coursera 

#created 31/01/2024
#online course covering various aspects of stats in R
#https://www.coursera.org/learn/introduction-statistics-data-analysis-public-health/home/week/1

#prep
library(xlsx)
library(tidyverse)

wd <- "C:/Users/elau2/OneDrive - NHS Digital/Documents/CPD/Learning"
setwd(wd)

#read in 
data <- read.csv(paste0(wd, "/Datasets/stats-in-r-cancer-data.csv"))

#exploration
summary(data$age)
hist(data$age)
table(data$smoking, exclude = NULL)

#exploration - fruit and veg consumption
data <- data %>%
  mutate(fruitveg = fruit+veg)

summary(data$fruitveg)
hist(data$fruitveg)

data <- data %>%
  mutate(five_a_day = ifelse(fruitveg >= 5, 1, 0))

table(data$five_a_day)

ggplot() + 
  geom_histogram(data = data, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") +
  labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  scale_y_continuous(limits = c(0,20))

#exploration - bmi
data <- data %>%
  mutate(bmi_category = case_when(bmi >= 18.5 & bmi <= 24.9 ~ "Healthy", 
                                            bmi < 18.5 ~ "Underweight",
                                            bmi > 24.9 ~ "Overweight")) %>%
  mutate(bmi_category = factor(bmi_category, levels = c("Underweight", "Healthy", "Overweight")))
                        

table(data$bmi_category)

ggplot() + 
  geom_bar(data = data, aes(x = bmi_category), fill = "darkred", col = "black") +
  labs(x = "BMI", y = "Frequency") +
  scale_y_continuous(limits = c(0,40))

#statistical testing 
chisq.test(x = data$five_a_day, y = data$cancer)
t.test(data$bmi~data$cancer) #comparing mean BMI for those with and without cancer
#twiddle symbol because cancer is categorical 

t.test(data$bmi, mu = 25) #testing null hypothesis than mean BMI is 25

#compare % overweight and cancer 
data <- data %>% mutate(overweight = ifelse(bmi_category == "Overweight", 1, 0))
chisq.test(x = data$overweight, y = data$cancer)
