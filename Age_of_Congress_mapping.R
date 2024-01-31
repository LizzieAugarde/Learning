##### Age of Congresspeople #####

#created 23/01/2024
#data from 538.com https://github.com/fivethirtyeight/data/tree/master/congress-demographics

#prep
library(tidyverse)
library(readxl)

raw_data <- read.csv("~/Learning/Datasets/data_aging_congress/data_aging_congress.csv")

#summary statistics by congressional session
hist(raw_data$age_years) #normally distributed, very slight positive skew

summary_session <- raw_data %>%
  group_by(congress) %>%
  summarize(min_value = min(age_years), 
            median = median(age_years),
            mean = mean(age_years), 
            sd_value = sd(age_years),
            max_value = max(age_years))

ggplot(summary_session, aes(x = congress, y = median)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#summary statistics by state
summary_state <- raw_data %>%
  group_by(state_abbrev) %>%
  summarize(min_value = min(age_years), 
            median = median(age_years),
            mean = mean(age_years), 
            sd_value = sd(age_years),
            max_value = max(age_years))

ggplot(summary_state, aes(x = state_abbrev, y = median)) + 
  geom_bar(stat = "identity")

#mapping - trying different methods
summary_state <- summary_state %>%
  mutate(state_name = tolower(state.name[match(state_abbrev, state.abb)]))

#1 - using ggplot
library(maps)
library(mapdata)

usa_states <- maps::map("state")

summary_state_ggplot <- merge(summary_state, usa_states, by.x = "state_name", by.y = "region")

ggplot(summary_state_ggplot, aes(x = long, y = lat, fill = median)) +
  geom_polygon(color = "black") 

#2 - using mapview
library(sf)
library(mapview)

usa_states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

summary_state_mapview <- left_join(usa_states_sf, summary_state, by = c("ID" = "state_name"))

mapview(summary_state_mapview, zcol = "median", color = "viridis", legend = TRUE)

#3 - using leaflet 
library(leaflet)
library(tigris)

usa_states_sf2 <- states(cb = T)

summary_state_leaf <- geo_join(usa_states_sf2, summary_state, "STUSPS", "state_abbrev")

summary_state_leaf <- subset(summary_state_leaf, !is.na(median))

pal <- colorNumeric("Greens", domain=summary_state_leaf$median)
popup <- paste0("State: ", summary_state_leaf$state_name, "Median age: ", summary_state_leaf$median)

summary_state_leaf %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(-98.483330, 38.712046, zoom = 4) %>% 
  addPolygons(data = summary_state_leaf , 
              fillColor = ~pal(summary_state_leaf$median), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup) %>%
  addLegend(pal = pal, 
            values = summary_state_leaf$median, 
            position = "bottomright", 
            title = "Age of Congress members")
  
