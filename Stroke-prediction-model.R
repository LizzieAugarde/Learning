##### Stroke prediction model beginner ML project #####

#06/11/2023 
#data from Kaggle https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset
#guidance from Analytics Vidhya https://www.analyticsvidhya.com/blog/2022/06/build-a-step-by-step-machine-learning-model-using-r/

#prep
library(tidyverse)
library(dplyr)
install.packages("caret", "randomForest")
library(caret) 
library(randomForest)

data <- read.csv("healthcare-dataset-stroke-data.csv")

#exploration and cleaning
summary(data)
unique(data$gender)
min(data$age)
hist(data$age)
under_1 <- filter(data, age<1)
other_gender <- filter(data, !gender %in% c("Male", "Female"))
sum(is.na(data))

install.packages("naniar") #fun package for dealing with NA and variations
library(naniar)

data <- data %>% 
  filter(gender %in% c("Male", "Female")) %>%
  replace_with_na_all(condition = ~.x == "N/A")

sum(is.na(data)) #201

data <- data %>%
  mutate(bmi = replace_na(as.numeric(bmi), mean(as.numeric(bmi), na.rm=TRUE))) %>% #replace missing BMI with mean BMI 
  mutate(stroke = as.factor(stroke))
  
sum(is.na(data)) #0

data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)

ggplot(data, aes(x = heart_disease, fill = stroke)) + geom_bar(stat = "count")
ggplot(data, aes(x = gender, fill = stroke)) + geom_bar(stat = "count")
ggplot(data, aes(x = Residence_type, fill = stroke)) + geom_bar(stat = "count")

#splitting into final and training datasets
obs <- nrow(data)
split <- round (obs*0.7)
training_set <- data[1:split,]
testing_set <- data[(split + 1):nrow(data),]
dim(training_set)

#initial model 
set.seed(123) #set random seed
model_1 <- randomForest(formula = stroke~., data = training_set)
model_1
