##### Learning web development keywords

#created 03/10/2023
#https://careerfoundry.com/en/blog/web-development/50-web-development-buzzwords-that-all-new-programmers-should-learn/


words <- c("Agile", "Algorithm", "API", "Application", "Adaptive design", "Boostrap", "Backend", "Browser", "Bug", "Cache", "Code", "CSS", "Data structure", "Debugging", "Deployment")

sample(words, 1, replace = FALSE, prob = NULL)


#version with webscraping
library(rvest)
library(tidyverse)

url <- "https://www.digitalsilk.com/digital-trends/web-development-terms/"

html <- read_html(url)

words <- html %>%
  html_nodes(".wp-block-heading") %>%
  html_text()

words <- as.data.frame(words) %>%
  rename(word = words) %>%
  filter(grepl("\\d+", word)) %>%
  mutate(word = str_replace(word, "^\\S* ", ""))

sample_n(words, size = 1)

