##### Webscraping #####

#created 06/11/2023 
#guidance from https://betterdatascience.com/r-web-scraping/?utm_content=cmp-true

library(rvest)

#practicing with books.toscrape.com
url <- "https://books.toscrape.com/catalogue/category/books/self-help_41/index.html"
titles <- read_html(url) %>%
  html_nodes('h3') %>%
  html_nodes('a') %>%
  html_text()

prices <- read_html(url) %>%
  html_nodes('.price_color') %>%
  html_text()

#scraping a dynamic table
url <- "https://www.worldometers.info/world-population/population-by-country/" 

html <- read_html(url)
table <- html_nodes(html, "table")
table_df <- table %>% html_table()
head(table_df)

#practicing with https://en.wikipedia.org/wiki/City_status_in_the_United_Kingdom
url <- "https://en.wikipedia.org/wiki/City_status_in_the_United_Kingdom"

html <- read_html(url)
tables <- html_nodes(html, ".wikitable") 
head(tables)
tables_list <- html %>% #extracting multiple tables from a single page
  html_nodes(".wikitable") %>%
  .[3:4] %>%
  html_table(fill = TRUE)

tables_list

#converting scraped tables to table objects
names(tables_list) <- c("smallest_overall_area", "largest_population")
list2env(tables_list, envir = .GlobalEnv)
