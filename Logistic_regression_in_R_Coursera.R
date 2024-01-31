##### Logistic regression in R Coursera 

#created 31/01/2024
#https://www.coursera.org/learn/logistic-regression-r-public-health

#prep
library(xlsx)
library(tidyverse)

wd <- "C:/Users/elau2/OneDrive - NHS Digital/Documents/CPD/Learning"
setwd(wd)

#read in 
data <- read.csv(paste0(wd, "/Datasets/logistic-regression-in-r-diabetes-data.csv"))

#exploration
table(data$gender, exclude = NULL) %>% addmargins()
round(prop.table(table(data$gender, exclude = NULL)), digits=3)

summary(data$chol)
summary(data$height)
summary(data$weight)

data <- data %>%
  mutate(height.si = height*0.0254,
         weight.si = weight*0.453592) %>% #need to convert height and weight to metric units
  mutate(bmi = weight.si/height.si^2) #creating bmi

summary(data$bmi)

data <- data %>%
  mutate(bmi_category = case_when(bmi >= 18.5 & bmi <= 24.9 ~ "Healthy", 
                                  bmi < 18.5 ~ "Underweight",
                                  bmi > 24.9 ~ "Overweight")) %>%
  mutate(bmi_category = factor(bmi_category, levels = c("Underweight", "Healthy", "Overweight")))

#cross tab BMI and diabetes status
dm_bmi_table <- table(data$bmi_category, factor(data$dm, exclude = NULL), exclude = NULL)

round(100 * prop.table(dm_bmi_table, margin = 1), digits = 1) 

#regression
data <- data %>%
  mutate(gender = as.factor(gender),
         dm = as.factor(dm))

model1 <- glm(data$dm ~ data$gender, family = binomial (link=logit))
summary(model1)
contrasts(data$gender) #see how R enters gender into the model
exp(model1$coefficients)

model2 <- glm(data$dm ~ data$age, family = binomial (link=logit))
summary(model2)
exp(model2$coefficients)

#assessing assumptions for age 
dm_by_age <- table(data$age, data$dm)
freq <- prop.table(dm_by_age, margin = 1) #frequencies of diabetes status by age
logodds <- log(freq[, "yes"]/freq[, "no"]) #log odds of diabetes
plot(rownames(freq), logodds) #plot ages against log odds of having diabetes

#looking at diabetes risk by location 
table(data$location, exclude = NULL)

dm_loc_table <- table(data$location, factor(data$dm, exclude = NA), exclude = NA)

round(100 * prop.table(dm_loc_table, margin = 1), digits = 1) 

data <- data %>% mutate(location = factor(location, levels = c("Buckingham", "Louisa")))

model3 <- glm(data$dm ~ data$location, family = binomial (link=logit))
summary(model3)
exp(model3$coefficients)
